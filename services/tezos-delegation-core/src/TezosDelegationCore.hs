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

cycleConsumer :: T -> Consumers CycleEvents
cycleConsumer T {db, amqp} =
  let nextCycle = selectNextCycle db
      getRewardInfo = makeClient amqp (Proxy :: Proxy RewardInfoEndpoint)
      f Tezos.CycleEvent {number = cycleNumber} = do
        wasCycleAlreadyHandled db cycleNumber >>= (`when` return ())
        delegates <- delegatesTrackedAtCycle db cycleNumber
        _ <- getRewardInfo $ RewardInfoRequest cycleNumber delegates
        return ()
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
         let t = T {db, amqp, redis, accessControlClient}
         f t)
