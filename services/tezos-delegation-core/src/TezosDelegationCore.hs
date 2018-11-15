module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

-- import TezosDelegationCore.Api as TezosDelegationCore
import qualified AccessControl.Client
import qualified Data.Yaml as Yaml
import qualified Postgres as Pg
import qualified Tezos
import TezosChainWatcher.Api
import TezosDelegationCore.Db
import TezosDelegationCore.Internal as TezosDelegationCore
import TezosHotWallet.Api
import Vest
import qualified Vest as CmdArgs (name)

data Args = Args
  { configDir :: FilePath
  , seedFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configDir =
        "services/config/local" &= help "Config directory" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    } &=
  help "Payout and refund server for the delegation marketplace." &=
  summary "tezos-delegation-core v0.1.0" &=
  program "tezos-delegation-core"

platformFee :: Rational
platformFee = 0.5

blockConsumer :: T -> Consumers BlockEvents
-- ^ On each cycle, bill each delegate and update dividends for each delegator.
-- Does not issue dividends; dividends are paid out when the delegator pays its bill. This happens
-- in the payment consumer.
blockConsumer t =
  let getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
      handleCycle blockNum cycleNum cycleTime = do
        Pg.runLogged t (wasCycleAlreadyHandled cycleNum) >>= (`when` return ())
        delegates <- Pg.runLogged t $ delegatesTrackedAtCycle cycleNum
        rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNum delegates
        time <- now
        forM_ rewardInfos $ \Tezos.RewardInfo { delegate
                                              , reward
                                              , stakingBalance
                                              , delegatedBalance
                                              , delegations
                                              } -> do
          Pg.runLogged t (wasRewardAlreadyProcessed cycleNum delegate) >>=
            (`when` return ())
          let delegateReward size =
                fixedOf Round $
                rationalOf reward ^* 0.1 ^* size ^/^ stakingBalance
              dividends_ =
                map
                  (\Tezos.DelegationInfo {delegator, size} ->
                     (delegator, delegateReward size))
                  delegations
              totalDividends =
                foldr
                  (\(_, size) tot -> tot + size)
                  (fixedQty @Mutez 0)
                  dividends_
              grossDelegateReward = reward - totalDividends
              vestCut =
                fixedOf Ceiling $ rationalOf grossDelegateReward ^* platformFee
              paymentOwed = totalDividends + vestCut
          billId <- nextUUID
          -- Insert entries into Pg
          Pg.runLoggedTransaction t $ do
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
            forM_ dividends_ $ \(delegator, size) ->
              Pg.runInsert $
              Pg.insert
                (dividends schema)
                (Pg.insertValues
                   [ Dividend
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       delegator
                       size
                       (BillId billId)
                       (PayoutId Nothing)
                       time
                   ])
                Pg.onConflictDefault
        Pg.runLogged t $
          Pg.runInsert $
          Pg.insert
            (cycles schema)
            (Pg.insertValues [Cycle cycleNum cycleTime blockNum blockNum time])
            Pg.onConflictDefault
      handleBlock Tezos.BlockEvent {number = blockNumber, cycleNumber, time} = do
        handleCycle blockNumber cycleNumber time
        Pg.runLogged t $
          Pg.runUpdate $
          Pg.update
            (cycles schema)
            (\Cycle {latestBlock} -> [latestBlock Pg.<-. Pg.val_ blockNumber])
            (\Cycle {number} -> number Pg.==. Pg.val_ cycleNumber)
   in (Pg.runLogged t selectNextBlock, handleBlock)

paymentConsumer :: T -> Consumers PaymentEvents
paymentConsumer t =
  let nextPayment = Pg.runLogged t selectNextPayment
      issuePayout = makeClient t (Proxy :: Proxy PayoutEndpoint)
      f PaymentEvent {idx, from, size, time = paymentTime} = do
        Pg.runLogged t (wasPaymentAlreadyHandled idx) >>= (`when` return ())
        isPlatformDelegate_ <- Pg.runLogged t $ isPlatformDelegate from
        time <- now
        when isPlatformDelegate_ $ do
          billsOutstanding_ <- Pg.runLogged t $ billsOutstanding $ retag from -- TODO: better address management for union of implicit and originated addresses
          let (billIdsPaid, refund) =
                foldr
                  (\Bill {id, size} (paid, rem) ->
                     if rem >= size
                       then (id : paid, rem - size)
                       else (paid, rem))
                  ([], size)
                  billsOutstanding_
          dividendsPaid <-
            foldr (<>) [] <$>
            mapM (Pg.runLogged t . dividendsForBill) billIdsPaid
          -- Issue dividends and record issuance
          -- TODO: could group dividends by delegator
          forM_ dividendsPaid $ \dividend@Dividend {delegator, size, payout} -> do
            when (isJust $ payoutId payout) $ return ()
            id <- nextUUID
            -- TODO: make sure this can't fail
            issuePayout $ PayoutRequest id (retag delegator) size
            Pg.runLoggedTransaction t $ do
              Pg.runInsert $
                Pg.insert
                  (payouts schema)
                  (Pg.insertValues [Payout id time])
                  Pg.onConflictDefault
              Pg.runUpdate $
                Pg.save
                  (dividends schema)
                  (dividend {payout = PayoutId $ Just id} :: Dividend)
          -- Mark bills paid
          Pg.runLogged t $
            Pg.runUpdate $
            Pg.update
              (bills schema)
              (\Bill {paidAt} -> [paidAt Pg.<-. Pg.val_ (Just paymentTime)])
              (\Bill {id} -> id `Pg.in_` (Pg.val_ <$> billIdsPaid))
          -- Issue refund if necessary
          when (refund > fixedQty @Mutez 0) $ do
            refundId <- nextUUID
            issuePayout $ PayoutRequest refundId from refund
            -- ^ TODO: make sure this can't fail
            Pg.runLoggedTransaction t $ do
              Pg.runInsert $
                Pg.insert
                  (payouts schema)
                  (Pg.insertValues [Payout refundId time])
                  Pg.onConflictDefault
              Pg.runInsert $
                Pg.insert
                  (refunds schema)
                  (Pg.insertValues
                     [Refund (PaymentIndex idx) refund (PayoutId refundId) time])
                  Pg.onConflictDefault
        Pg.runLogged t $
          Pg.runInsert $
          Pg.insert
            (payments schema)
            (Pg.insertValues [Payment idx time])
            Pg.onConflictDefault
   in (nextPayment, f)

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = BlockEvents
                          :<|> PaymentEvents
  type RpcSpec T = ()
  defaultArgs = defaultArgs_
  init Args {configDir, seedFile} f = do
    accessControlPublicKey <- load configDir
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    withLoadable configDir $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, accessControlClient}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers t = blockConsumer t :<|> paymentConsumer t
