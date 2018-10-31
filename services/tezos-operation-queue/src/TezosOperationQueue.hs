-- Currently intended as an executable for mocked testing data.
module TezosOperationQueue
  ( module TezosOperationQueue
  ) where

-- import TezosOperationQueue.Api as TezosOperationQueue
import qualified Data.Yaml as Yaml
import Db
import TezosOperationQueue.Internal as TezosOperationQueue
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: PostgresConfig
  -- , amqpConfig :: Amqp.Config
  , webSocketConfig :: WebSocket.Config
  -- , redisConfig :: RedisConfig
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
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
  help "RPC queue for Tezos operations signed on the front-end." &=
  summary "tezos-operation-queue v0.1.0" &=
  program "tezos-operation-queue"

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
