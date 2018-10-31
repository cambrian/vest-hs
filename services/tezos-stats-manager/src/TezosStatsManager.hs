-- Currently intended as an executable for mocked testing data.
module TezosStatsManager
  ( module TezosStatsManager
  ) where

-- import TezosStatsManager.Api as TezosStatsManager
-- import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import Db
import TezosStatsManager.Internal as TezosStatsManager

-- import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: PostgresConfig
  , webSocketConfig :: WebSocket.Config
  -- , amqpConfig :: Amqp.Config
  -- , redisConfig :: RedisConfig
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
  -- , seedFile :: FilePath
  -- , accessControlPublicKeyFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configFile =
        "config.yaml" &= help "YAML config file" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    -- , seedFile =
    --     "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
    --     CmdArgs.name "d" &=
    --     typFile
    -- , accessControlPublicKeyFile =
    --     "access-control-public-key.yaml" &=
    --     help "YAML access control public key file" &=
    --     explicit &=
    --     CmdArgs.name "key" &=
    --     CmdArgs.name "k" &=
    --     typFile
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
  init Args {configFile} f = do
    Config {dbConfig, webSocketConfig} <- Yaml.decodeFileThrow configFile
    with2 dbConfig webSocketConfig (\(db, webSocket) -> f $ T {db, webSocket})
