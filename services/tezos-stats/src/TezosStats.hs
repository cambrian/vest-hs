-- Currently intended as an executable for mocked testing data.
module TezosStats
  ( module TezosStats
  ) where

import TezosStats.Api as TezosStats

-- import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import Db
import TezosStats.Internal as TezosStats

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
  summary "tezos-stats v0.1.0" &=
  program "tezos-stats"

type Api
   = OverviewEndpoint
     :<|> BakersEndpoint
     :<|> ImplicitEndpoint
     :<|> OperationEndpoint

handlers :: Handlers ()
handlers = ()

type AuxiliaryTypes
   = Raw Baker
     :<|> Raw DelegateFraction
     :<|> Raw DelegateInfo
     :<|> Raw LedgerOperation
     :<|> Raw LedgerOperationType
     :<|> Raw OriginatedAccount
     :<|> Raw TimestampRate
     :<|> Raw TimestampSize

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = ()
  defaultArgs = defaultArgs_
  init Args {configFile} f = do
    Config {dbConfig, webSocketConfig} <- Yaml.decodeFileThrow configFile
    with2 dbConfig webSocketConfig (\(db, webSocket) -> f $ T {db, webSocket})
