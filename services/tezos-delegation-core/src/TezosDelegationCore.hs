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

type Api = ()

handlers :: Handlers Api
handlers = ()

platformFee :: Rational
platformFee = 0.5

billPaymentTime :: Time Day
billPaymentTime = day 5

makeCycleConsumer :: T -> Consumers CycleEvents
makeCycleConsumer t@T {db} =
  let nextCycle = selectNextCycle db
      getRewardInfo = makeClient t (Proxy :: Proxy RewardInfoEndpoint)
      f Tezos.CycleEvent {number = cycleNo, timestamp} = do
        wasCycleAlreadyHandled db cycleNo >>= (`when` return ())
        delegates <- delegatesTrackedAtCycle db cycleNo
        rewardInfos <- getRewardInfo $ RewardInfoRequest cycleNo delegates
        time <- now
        -- Wrap the whole cycle's processing in a transaction because it lets us not deal with
        -- partially processed cycles.
        Db.withTransactionSerializable db $ do
          forM_ rewardInfos $ \Tezos.RewardInfo { delegate
                                                , reward
                                                , stakingBalance
                                                , delegatedBalance
                                                , delegations
                                                } -> do
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
                  fixedOf Ceiling $
                  rationalOf grossDelegateReward ^* platformFee
                paymentOwed = totalDividends + vestCut
            billId <- nextUUID
            -- Insert entries into Db
            Db.runBeamPostgresDebug (log t Debug . pack) db $ do
              Db.runInsert $
                Db.insert
                  (bills schema)
                  (Db.insertValues
                     [ Bill
                         billId
                         (DelegateAddress delegate)
                         paymentOwed
                         True
                         False
                         timestamp
                         (timeAdd billPaymentTime time)
                         time
                     ])
                  Db.onConflictDefault
              Db.runInsert $
                Db.insert
                  (rewards schema)
                  (Db.insertValues
                     [ Reward
                         (DelegateAddress delegate)
                         (CycleNumber cycleNo)
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
                         delegator
                         (DelegateAddress delegate)
                         (CycleNumber cycleNo)
                         size
                         (BillId billId)
                         (PayoutId Nothing)
                         time
                     ])
                  Db.onConflictDefault
          Db.runBeamPostgresDebug (log t Debug . pack) db $ -- TODO: short hand for this
            Db.runInsert $
            Db.insert
              (cycles schema)
              (Db.insertValues [Cycle cycleNo timestamp time])
              Db.onConflictDefault
   in (nextCycle, f)

makePaymentConsumer :: T -> Consumers PaymentEvents
makePaymentConsumer t@T {db} =
  let nextPayment = selectNextPayment db
      issuePayout = makeClient t (Proxy :: Proxy PayoutEndpoint)
      f PaymentEvent {idx, from, size} = do
        wasPaymentAlreadyHandled db idx >>= (`when` return ())
        isPlatformDelegate_ <- isPlatformDelegate db from
        time <- now
        when isPlatformDelegate_ $ do
          billsOutstanding_ <- billsOutstanding db $ retag from -- TODO: better address management
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
            mapM (\Bill {id} -> dividendsForBill db id) billsPaid
          -- TODO: could group dividends by delegator
          forM_ dividendsPaid $ \dividend@Dividend {delegator, size, payout} -> do
            when (isJust $ payoutId payout) $ return ()
            id <- nextUUID
            -- TODO: make sure this can't fail
            issuePayout $ PayoutRequest id (retag delegator) size
            Db.runBeamPostgresDebug (log t Debug . pack) db $ do
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
            Db.runBeamPostgresDebug (log t Debug . pack) db $ do
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
        Db.runBeamPostgresDebug (log t Debug . pack) db $
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
  type EventsConsumed T = CycleEvents
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, redisConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    with3
      dbConfig
      amqpConfig
      redisConfig
      (\(db, amqp, redis) -> do
         accessControlClient <-
           AccessControl.Client.make amqp accessControlPublicKey seed
         f $ T {db, amqp, redis, accessControlClient})
