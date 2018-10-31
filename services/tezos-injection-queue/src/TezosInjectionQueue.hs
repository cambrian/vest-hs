module TezosInjectionQueue
  ( module TezosInjectionQueue
  ) where

-- import TezosInjectionQueue.Api as TezosInjectionQueue
import qualified Data.Yaml as Yaml
import Db
import qualified Http
import TezosInjectionQueue.Internal as TezosInjectionQueue
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: PostgresConfig
  , webSocketConfig :: WebSocket.Config
  , tezosConfig :: Http.Config
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
    } &=
  help "RPC queue for Tezos operations signed on the front-end." &=
  summary "tezos-injection-queue v0.1.0" &=
  program "tezos-injection-queue"

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
    Config {dbConfig, webSocketConfig, tezosConfig} <-
      Yaml.decodeFileThrow configFile
    with3
      dbConfig
      webSocketConfig
      tezosConfig
      (\(db, webSocket, tezos) -> f $ T {db, webSocket, tezos})
