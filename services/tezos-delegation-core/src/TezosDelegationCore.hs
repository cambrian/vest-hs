module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

-- import TezosDelegationCore.Api as TezosDelegationCore
import qualified AccessControl.Client
import qualified Data.Yaml as Yaml
import qualified Db
import qualified Tezos
import TezosChainWatcher.Api
import TezosDelegationCore.Db
import TezosDelegationCore.Internal as TezosDelegationCore
import TezosHotWallet.Api
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: Db.Config
  , amqpConfig :: Amqp.Config
  , redisConfig :: RedisConfig
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
  , seedFile :: FilePath
  , accessControlPublicKeyFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configFile =
        "config.yaml" &= help "YAML config file" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    , accessControlPublicKeyFile =
        "access-control-public-key.yaml" &=
        help "YAML access control public key file" &=
        explicit &=
        CmdArgs.name "key" &=
        CmdArgs.name "k" &=
        typFile
    } &=
  help "Payout and refund server for the delegation marketplace." &=
  summary "tezos-delegation-core v0.1.0" &=
  program "tezos-delegation-core"

platformFee :: Rational
platformFee = 0.5

billPaymentTime :: Duration
billPaymentTime = day 5

blockConsumer :: T -> Consumers BlockEvents
-- ^ On each cycle, bill each delegate and update dividends for each delegator.
-- Does not issue dividends; dividends are paid out when the delegator pays its bill. This happens
-- in the payment consumer.
-- TODO: check late bills
blockConsumer t =
  let getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
      handleCycle blockNum cycleNum cycleTime = do
        Db.runLogged t (wasCycleAlreadyHandled cycleNum) >>= (`when` return ())
        delegates <- Db.runLogged t $ delegatesTrackedAtCycle cycleNum
        rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNum delegates
        time <- now
        forM_ rewardInfos $ \Tezos.RewardInfo { delegate
                                              , reward
                                              , stakingBalance
                                              , delegatedBalance
                                              , delegations
                                              } -> do
          Db.runLogged t (wasRewardAlreadyProcessed cycleNum delegate) >>=
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
          -- Insert entries into Db
          Db.runLoggedTransaction t $ do
            Db.runInsert $
              Db.insert
                (bills schema)
                (Db.insertValues
                   [ Bill
                       billId
                       (DelegateAddress delegate)
                       paymentOwed
                       time
                       Nothing
                   ])
                Db.onConflictDefault
            Db.runInsert $
              Db.insert
                (rewards schema)
                (Db.insertValues
                   [ Reward
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       reward
                       stakingBalance
                       delegatedBalance
                       (BillId billId)
                       time
                   ])
                Db.onConflictDefault
            forM_ dividends_ $ \(delegator, size) ->
              Db.runInsert $
              Db.insert
                (dividends schema)
                (Db.insertValues
                   [ Dividend
                       (CycleNumber cycleNum)
                       (DelegateAddress delegate)
                       delegator
                       size
                       (BillId billId)
                       (PayoutId Nothing)
                       time
                   ])
                Db.onConflictDefault
        Db.runLogged t $
          Db.runInsert $
          Db.insert
            (cycles schema)
            (Db.insertValues [Cycle cycleNum cycleTime blockNum blockNum time])
            Db.onConflictDefault
      handleBlock Tezos.BlockEvent {number = blockNumber, cycleNumber, time} = do
        handleCycle blockNumber cycleNumber time
        Db.runLogged t $
          Db.runUpdate $
          Db.update
            (cycles schema)
            (\cycle -> [latestBlock cycle Db.<-. Db.val_ blockNumber])
            (\cycle -> number cycle Db.==. Db.val_ cycleNumber)
   in (Db.runLogged t selectNextBlock, handleBlock)

paymentConsumer :: T -> Consumers PaymentEvents
paymentConsumer t =
  let nextPayment = Db.runLogged t selectNextPayment
      issuePayout = makeClient t (Proxy :: Proxy PayoutEndpoint)
      f PaymentEvent {idx, from, size} = do
        Db.runLogged t (wasPaymentAlreadyHandled idx) >>= (`when` return ())
        isPlatformDelegate_ <- Db.runLogged t $ isPlatformDelegate from
        time <- now
        when isPlatformDelegate_ $ do
          billsOutstanding_ <- Db.runLogged t $ billsOutstanding $ retag from -- TODO: better address management for union of implicit and originated addresses
          let (billsPaid, refund) =
                foldr
                  (\b@Bill {size} (paid, rem) ->
                     if rem >= size
                       then (b : paid, rem - size)
                       else (paid, rem))
                  ([], size)
                  billsOutstanding_
          dividendsPaid <-
            foldr (<>) [] <$>
            mapM (\Bill {id} -> Db.runLogged t $ dividendsForBill id) billsPaid
          -- TODO: could group dividends by delegator
          forM_ dividendsPaid $ \dividend@Dividend {delegator, size, payout} -> do
            when (isJust $ payoutId payout) $ return ()
            id <- nextUUID
            -- TODO: make sure this can't fail
            issuePayout $ PayoutRequest id (retag delegator) size
            Db.runLoggedTransaction t $ do
              Db.runInsert $
                Db.insert
                  (payouts schema)
                  (Db.insertValues [Payout id time])
                  Db.onConflictDefault
              Db.runUpdate $
                Db.save
                  (dividends schema)
                  (dividend {payout = PayoutId $ Just id} :: Dividend)
          -- Issue refund if necessary
          when (refund > fixedQty @Mutez 0) $ do
            refundId <- nextUUID
            issuePayout $ PayoutRequest refundId from refund
            -- ^ TODO: make sure this can't fail
            Db.runLoggedTransaction t $ do
              Db.runInsert $
                Db.insert
                  (payouts schema)
                  (Db.insertValues [Payout refundId time])
                  Db.onConflictDefault
              Db.runInsert $
                Db.insert
                  (refunds schema)
                  (Db.insertValues
                     [Refund (PaymentIndex idx) refund (PayoutId refundId) time])
                  Db.onConflictDefault
        Db.runLogged t $
          Db.runInsert $
          Db.insert
            (payments schema)
            (Db.insertValues [Payment idx time])
            Db.onConflictDefault
   in (nextPayment, f)

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = BlockEvents
                          :<|> PaymentEvents
  type RpcSpec T = ()
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, redisConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    with (PoolConfig (sec 30) 5 dbConfig :<|> amqpConfig :<|> redisConfig) $ \(dbPool :<|> amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, accessControlClient}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers t = blockConsumer t :<|> paymentConsumer t
