module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api
import TezosDelegationCore.Db
import TezosDelegationCore.Internal as TezosDelegationCore
import TezosHotWallet.Api
import Vest

newtype PlatformFee =
  PlatformFee Rational
  deriving newtype (Show, FromJSON)

instance Loadable PlatformFee where
  configFile = [relfile|platform-fee.yaml|]

blockConsumer :: T -> Consumers FinalizedBlockEvents
-- ^ On each cycle, bill each delegate and update dividends for each delegator.
-- Does not issue dividends; the relevant dividends are paid out when a delegator pays its bill.
-- This happens in the payment consumer.
blockConsumer t@T {dbPool, platformFee} =
  let getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
      dividendSize reward stakingBalance priceTiers sizeDelegated =
        let (rationalSize, _) =
              foldl'
                (\(div, rem) (tierStart, rate) ->
                   if tierStart > rem
                     then (div, rem)
                     else ( div +
                            rationalOf reward ^*
                            (rate * ((rem - tierStart) ^/^ stakingBalance))
                          , tierStart))
                (0, sizeDelegated)
                priceTiers
         in fixedOf Floor rationalSize
      handleCycle blockNum cycleNum cycleTime = do
        Pg.runLogged dbPool (Pg.wasHandled (cycles schema) cycleNum) >>=
          (`when` return ())
        delegates <- Pg.runLogged dbPool $ delegatesTrackedAtCycle cycleNum
        rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNum delegates
        time <- now
        forM_ rewardInfos $ \Tezos.RewardInfo { delegate
                                              , reward
                                              , stakingBalance
                                              , delegatedBalance
                                              , delegations
                                              } -> do
          Pg.runLogged dbPool (wasRewardAlreadyProcessed cycleNum delegate) >>=
            (`when` return ())
          priceTiers <- Pg.runLogged dbPool $ getDelegatePriceTiers delegate
          billId <- nextUUID
          let dividends_ =
                map
                  (\Tezos.DelegationInfo {delegator, size} ->
                     Dividend
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       delegator
                       (dividendSize reward stakingBalance priceTiers size)
                       (BillId billId)
                       (PayoutId Nothing)
                       time)
                  delegations
              totalDividends =
                foldr (\Dividend {size} tot -> tot + size) 0 dividends_
              grossDelegateReward = reward - totalDividends
              vestCut =
                fixedOf Ceiling $ rationalOf grossDelegateReward ^* platformFee
              paymentOwed = totalDividends + vestCut
          -- Persist reward/bill/dividends.
          Pg.runLoggedTransaction dbPool $ do
            Pg.runInsert $
              Pg.insert
                (bills schema)
                (Pg.insertValues
                   [ Bill
                       billId
                       (DelegateAddress delegate)
                       paymentOwed
                       time
                       Nothing
                   ])
                Pg.onConflictDefault
            Pg.runInsert $
              Pg.insert
                (rewards schema)
                (Pg.insertValues
                   [ Reward
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       reward
                       stakingBalance
                       delegatedBalance
                       (BillId billId)
                       time
                   ])
                Pg.onConflictDefault
            Pg.runInsert $
              Pg.insert
                (dividends schema)
                (Pg.insertValues dividends_)
                Pg.onConflictDefault
        -- Mark cycle as processed.
        Pg.runLogged dbPool $
          Pg.runInsert $
          Pg.insert
            (cycles schema)
            (Pg.insertValues [Cycle cycleNum cycleTime blockNum blockNum time])
            Pg.onConflictDefault
      handleBlock Tezos.BlockEvent {number = blockNumber, cycleNumber, time} = do
        handleCycle blockNumber cycleNumber time
        -- Mark block as processed.
        Pg.runLogged dbPool $
          Pg.runUpdate $
          Pg.update
            (cycles schema)
            (\Cycle {latestBlock} -> [latestBlock Pg.<-. Pg.val_ blockNumber])
            (\Cycle {number} -> number Pg.==. Pg.val_ cycleNumber)
   in (Pg.runLogged dbPool selectNextBlock, handleBlock)

paymentConsumer :: T -> Consumers PaymentEvents
paymentConsumer t@T {dbPool, operationFee} =
  let nextPayment = Pg.runLogged dbPool $ Pg.nextUnseenIndex $ payments schema
      issuePayouts = makeClient t (Proxy :: Proxy PayoutEndpoint)
      f PaymentEvent {idx, from, size, time = paymentTime} = do
        Pg.runLogged dbPool (Pg.wasHandled (payments schema) idx) >>=
          (`when` return ())
        isPlatformDelegate_ <- Pg.runLogged dbPool $ isPlatformDelegate from
        time <- now
        fee <-
          readLatestValue operationFee >>=
          fromJustUnsafe (MissingValueException @OperationFeeValue)
        billsOutstanding_ <-
          Pg.runLogged dbPool $ billsOutstanding $ Tagged from
        let (billIdsPaid, refundSize) =
              foldr
                (\Bill {id, size} (paid, rem) ->
                   if rem >= size
                     then (id : paid, rem - size)
                     else (paid, rem))
                ([], size)
                billsOutstanding_
            shouldRefund = isPlatformDelegate_ && refundSize > 0
            -- ^ Note: in the future we may want more sophisticated logic re. when to make refunds
        dividendsPaid <-
          foldr (<>) [] <$>
          mapM (Pg.runLogged dbPool . dividendsForBill) billIdsPaid
        newDividends <-
          mapM
            (\dividend -> do
               payout <- PayoutId . Just <$> nextUUID
               return (dividend {payout} :: Dividend))
            dividendsPaid
        let dividends_ =
              map
                (\Dividend {delegator = Tagged to, size, payout = PayoutId id} ->
                   PayoutRequest
                     {id = fromMaybe (panic "impossible") id, to, size})
                newDividends
        refundId <- nextUUID -- only used if shouldRefund = True
        refund <-
          if shouldRefund
            then return [PayoutRequest refundId from refundSize]
            else return []
        let payouts_ = refund <> dividends_
        issuePayouts (payouts_, fee)
        Pg.runLoggedTransaction dbPool $ do
          Pg.runInsert $
            Pg.insert
              (payouts schema)
              (Pg.insertValues $
               map
                 (\PayoutRequest {id, to, size} -> Payout id to size fee time)
                 payouts_)
              Pg.onConflictDefault
          Pg.runInsert $
            Pg.insert (dividends schema) (Pg.insertValues newDividends) $
            Pg.onConflict
              (Pg.conflictingFields Pg.primaryKey)
              Pg.onConflictSetAll
          Pg.runUpdate $
            Pg.update
              (bills schema)
              (\Bill {paidAt} -> [paidAt Pg.<-. Pg.val_ (Just paymentTime)])
              (\Bill {id} -> id `Pg.in_` (Pg.val_ <$> billIdsPaid))
          Pg.runInsert $
            Pg.insert
              (payments schema)
              (Pg.insertValues
                 [ Payment
                     idx
                     (PayoutId $
                      if shouldRefund
                        then Just refundId
                        else Nothing)
                     time
                 ])
              Pg.onConflictDefault
   in (nextPayment, f)

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = FinalizedBlockEvents
                          :<|> PaymentEvents
  type RpcSpec T = ()
  summary = "Tezos Delegation Core v0.1.0"
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
  eventConsumers t = blockConsumer t :<|> paymentConsumer t
