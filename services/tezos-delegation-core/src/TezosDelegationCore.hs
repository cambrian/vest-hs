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

cycleConsumer :: T -> Consumers CycleEvents
cycleConsumer t@T {db, amqp} =
  let nextCycle = selectNextCycle db
      getRewardInfo = makeClient amqp (Proxy :: Proxy RewardInfoEndpoint)
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
            dividends_ <-
              mapM
                (\Tezos.DelegationInfo {delegator, size} -> do
                   id <- nextUUID
                   -- TODO: adjust for overdelegation
                   -- TODO: use delegate price tiers
                   let d =
                         fixedOf Round $
                         rationalOf reward ^* 0.1 ^* size ^/^ stakingBalance
                   return (id, delegator, d))
                delegations
            let totalDividends =
                  foldr
                    (\(_, _, size) tot -> tot + size)
                    (fixedQty @Mutez 0)
                    dividends_
                grossDelegateReward = reward - totalDividends
                paymentOwed =
                  fixedOf Ceiling $
                  rationalOf grossDelegateReward ^* platformFee
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
                         (fixedQty @Mutez 0)
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
                         timestamp
                         time
                     ])
                  Db.onConflictDefault
              forM_ dividends_ $ \(id, delegator, size) ->
                Db.runInsert $
                Db.insert
                  (dividends schema)
                  (Db.insertValues
                     [ Dividend
                         id
                         delegator
                         (DelegateAddress delegate)
                         size
                         timestamp
                         time
                     ])
                  Db.onConflictDefault
          Db.runBeamPostgresDebug (log t Debug . pack) db $
            Db.runInsert $
            Db.insert
              (cycles schema)
              (Db.insertValues [Cycle cycleNo timestamp time])
              Db.onConflictDefault
   in (nextCycle, f)

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
