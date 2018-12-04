module TezosPlatformCore
  ( module TezosPlatformCore
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api
import qualified TezosInjector.Api as TezosInjector
import TezosPlatformCore.Db
import TezosPlatformCore.Internal as TezosPlatformCore
import Vest

newtype PlatformFee =
  PlatformFee Rational
  deriving newtype (Show, FromJSON)

instance Loadable PlatformFee where
  configFile = [relfile|platform-fee.yaml|]

handleCycle :: T -> Tezos.BlockEvent -> IO ()
handleCycle t@T {dbPool, platformFee} Tezos.BlockEvent { number = blockNumber
                                                       , cycleNumber
                                                       } = do
  alreadyHandled <-
    Pg.runLogged dbPool $ Pg.wasHandled (cycles schema) cycleNumber
  unless alreadyHandled $ do
    let getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
        dividendSize reward stakingBalance priceTiers sizeDelegated =
          let (rationalSize, _) =
                foldl'
                  (\(div, rem) (tierStart, rate) ->
                     if tierStart > rem
                       then (div, rem)
                       else ( div +
                              rationalOf reward ^*
                              ((1 - rate) *
                               ((rem - tierStart) ^/^ stakingBalance))
                            , tierStart))
                  (0, sizeDelegated)
                  priceTiers
           in fixedOf Floor rationalSize
    delegates <- Pg.runLogged dbPool $ delegatesTrackedAtBlock blockNumber
    rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNumber delegates
    time <- now
    (allRewards, allDividends) <-
      do rewardsAndDividends <-
           mapM
             (\Tezos.RewardInfo { delegate
                                , reward
                                , stakingBalance
                                , delegatedBalance
                                , delegations
                                } -> do
                priceTiers <-
                  Pg.runLogged dbPool $ getDelegatePriceTiers delegate
                platformDelegations <-
                  filterM
                    (\Tezos.DelegationInfo {delegator} ->
                       Pg.runLogged dbPool $
                       isPlatformDelegation delegate delegator) $
                  delegations
                let dividends =
                      filter (\Dividend {size} -> size > 0) $
                      map
                        (\Tezos.DelegationInfo {delegator, size} ->
                           Dividend
                             { reward =
                                 RewardPKey
                                   (CycleNumber cycleNumber)
                                   (DelegateAddress delegate)
                             , delegator
                             , size =
                                 dividendSize
                                   reward
                                   stakingBalance
                                   priceTiers
                                   size
                             , payout = PayoutId Nothing
                             , created_at = time
                             })
                        platformDelegations
                    totalDividendSize =
                      foldr (\Dividend {size} tot -> tot + size) 0 dividends
                    grossDelegateReward = reward - totalDividendSize
                    platformCharge =
                      fixedOf Ceiling $
                      rationalOf grossDelegateReward ^* platformFee
                    payment_owed = totalDividendSize + platformCharge
                    reward_ =
                      Reward
                        { cycle = CycleNumber cycleNumber
                        , delegate = DelegateAddress delegate
                        , size = reward
                        , staking_balance = stakingBalance
                        , delegated_balance = delegatedBalance
                        , payment_owed
                        , payment = PaymentHash Nothing
                        , created_at = time
                        }
                return (reward_, dividends))
             rewardInfos
         let (rs, ds) = unzip rewardsAndDividends
         return (rs, foldr (<>) [] ds)
    -- Persist reward/dividends, mark cycle as processed
    Pg.runLoggedTransaction dbPool $ do
      Pg.runInsert $
        Pg.insert
          (cycles schema)
          (Pg.insertValues
             [ Cycle
                 { number = cycleNumber
                 , first_block = BlockNumber blockNumber
                 , created_at = time
                 }
             ])
          Pg.onConflictDefault
      Pg.runInsert $
        Pg.insert
          (rewards schema)
          (Pg.insertValues allRewards)
          Pg.onConflictDefault
      Pg.runInsert $
        Pg.insert
          (dividends schema)
          (Pg.insertValues allDividends)
          Pg.onConflictDefault

handleDelegation :: T -> Tezos.Transaction -> IO ()
handleDelegation T {dbPool} Tezos.Transaction {from, to} = do
  time <- now
  void . runMaybeT $ do
    delegate <-
      MaybeT $ Pg.runLogged dbPool $ delegateOfDummyAddress $ Tagged to
    lift $
      Pg.runLogged dbPool $
      Pg.runInsert $
      Pg.insert
        (delegations schema)
        (Pg.insertValues
           [ Delegation
               { delegate = DelegateAddress delegate
               , delegator = Tagged from
               , created_at = time
               }
           ]) $
      Pg.onConflict Pg.anyConflict Pg.onConflictDoNothing

handlePayment :: T -> Word64 -> Tezos.Transaction -> IO ()
handlePayment t@T {dbPool, operationFee} blockNumber Tezos.Transaction { hash
                                                                       , from
                                                                       , to
                                                                       , size
                                                                       } = do
  alreadyHandled <- Pg.runLogged dbPool $ wasPaymentHandled hash
  fromPlatformDelegate <- Pg.runLogged dbPool $ isPlatformDelegate from
  toVest <- return $ to == panic "TODO"
  when (toVest && fromPlatformDelegate && not alreadyHandled) $ do
    let issuePayouts =
          makeClient t (Proxy :: Proxy TezosInjector.PayoutEndpoint)
    time <- now
    fee <-
      readLatestValue operationFee >>=
      fromJustUnsafe (MissingValueException @OperationFeeValue)
    rewardsUnpaid_ <- Pg.runLogged dbPool $ rewardsUnpaid $ Tagged from
    let (rewardsPaid, refundSize) =
          foldr
            (\reward@Reward {payment_owed} (paid, rem) ->
               if rem >= payment_owed
                 then (reward : paid, rem - payment_owed)
                 else (paid, rem))
            ([], size)
            rewardsUnpaid_
    dividendsPaid <-
      foldr (<>) [] <$>
      mapM (Pg.runLogged dbPool . dividendsForReward) rewardsPaid
    updatedDividends <-
      mapM
        (\dividend -> do
           payout <- PayoutId . Just <$> nextUUID
           return (dividend {payout} :: Dividend))
        dividendsPaid
    refundId <- nextUUID -- only used if refundSize > 0
    let dividendPayouts =
          map
            (\Dividend {delegator = Tagged to, size, payout = PayoutId id} ->
               TezosInjector.Payout
                 {id = fromMaybe (panic "impossible") id, to, size})
            updatedDividends
        refundPayouts =
          [ TezosInjector.Payout {id = refundId, to = from, size = refundSize}
          | refundSize > 0
          ]
        allPayouts = refundPayouts <> dividendPayouts
    issuePayouts $ TezosInjector.BatchPayout allPayouts fee
    Pg.runLoggedTransaction dbPool $ do
      Pg.runInsert $
        Pg.insert
          (payouts schema)
          (Pg.insertValues $
           map
             (\TezosInjector.Payout {id, to, size} ->
                Payout {id, to, size, fee, created_at = time})
             allPayouts)
          Pg.onConflictDefault
      Pg.runInsert $
        Pg.insert
          (payments schema)
          (Pg.insertValues
             [ Payment
                 { hash
                 , from
                 , size
                 , block = BlockNumber blockNumber
                 , refund =
                     PayoutId $
                     if refundSize > 0
                       then Just refundId
                       else Nothing
                 , created_at = time
                 }
             ])
          Pg.onConflictDefault
      Pg.runInsert $
        Pg.insert (dividends schema) (Pg.insertValues updatedDividends) $
        Pg.onConflict (Pg.conflictingFields Pg.primaryKey) Pg.onConflictSetAll
      Pg.runInsert $
        Pg.insert
          (rewards schema)
          (Pg.insertValues $
           fmap
             (\reward -> reward {payment = PaymentHash $ Just hash})
             rewardsPaid) $
        Pg.onConflict (Pg.conflictingFields Pg.primaryKey) Pg.onConflictSetAll

blockConsumer :: T -> Consumers FinalizedBlockEvents
-- ^ On each cycle, bill each delegate and update dividends for each delegator.
-- Does not issue dividends; the relevant dividends are paid out when a delegator pays its bill.
-- This happens in the payment consumer.
blockConsumer t@T {dbPool} =
  let handleBlock blockEvent@Tezos.BlockEvent { number = blockNumber
                                              , time = blockTime
                                              , transactions
                                              } = do
        time <- now
        Pg.runLogged dbPool $
          Pg.runInsert $
          Pg.insert
            (blocks schema)
            (Pg.insertValues
               [ Block
                   { number = blockNumber
                   , time = blockTime
                   , created_at = time
                   , handled = False
                   }
               ]) $
          Pg.onConflict Pg.anyConflict Pg.onConflictDoNothing
        handleCycle t blockEvent
        forM_ transactions $ \tx ->
          handleDelegation t tx >> handlePayment t blockNumber tx
        Pg.runLogged dbPool $
          Pg.runUpdate $
          Pg.update
            (blocks schema)
            (\Block {handled} -> [handled Pg.<-. Pg.val_ True])
            (\Block {number} -> number Pg.==. Pg.val_ blockNumber)
   in (Pg.runLogged dbPool selectNextUnhandledBlock, handleBlock)

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = FinalizedBlockEvents
  type RpcSpec T = ()
  summary = "Tezos Platform Core v0.1.0"
  description =
    "Tracks how much delegates owe their delegators for each reward event, and issues payouts on behalf of delegates."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> PlatformFee platformFee) <-
      load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      operationFee <- subscribe amqp (Proxy :: Proxy OperationFeeValue)
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      f $
        T {dbPool, amqp, redis, accessControlClient, operationFee, platformFee}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers = blockConsumer
