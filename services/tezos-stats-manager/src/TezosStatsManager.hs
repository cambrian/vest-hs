module TezosStatsManager
  ( module TezosStatsManager
  ) where

-- import TezosStatsManager.Api as TezosStatsManager
import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import qualified Db
import TezosStatsManager.Internal as TezosStatsManager
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: Db.JsonConfig
  , webSocketConfig :: WebSocket.Config
  , amqpConfig :: Amqp.Config
  , redisConfig :: RedisJsonConfig
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
  help "Front-end stats server for Tezos." &=
  summary "tezos-stats-manager v0.1.0" &=
  program "tezos-stats-manager"

type Api = ()

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, webSocketConfig, redisConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    let dbConfig_ = Db.toConfig dbConfig
    let redisConfig_ = toRedisConfig redisConfig
    with4
      dbConfig_
      amqpConfig
      webSocketConfig
      redisConfig_
      (\(db, amqp, webSocket, redis) ->
         with
           AccessControlClient.Config {accessControlPublicKey, seed, amqp}
           (\accessControlClient ->
              f $ T {db, amqp, webSocket, redis, accessControlClient}))
