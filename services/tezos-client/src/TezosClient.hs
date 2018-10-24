module TezosClient
  ( module TezosClient
  ) where

import qualified Data.ByteString as ByteString
import qualified Http

-- import TezosClient.Api as TezosClient
import TezosClient.Internal as TezosClient
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Args = Args
  { port :: Int16
  , seedFile :: Text
  , tezosHost :: Text
  , tezosPort :: Int16
  , tezosPath :: Text
  , secure :: Bool
  } deriving (Eq, Show, Read, Generic, Data)

-- Normally these would go in a YAML file, but since the front-end will be testing directly with
-- this executable, the config is exposed as command-line arguments.
_defaultArgs :: Args
_defaultArgs =
  Args
    { port =
        3000 &= help "Port to serve on" &= CmdArgs.name "port" &=
        CmdArgs.name "p" &=
        typ "PORT"
    , seedFile =
        "seed.yaml" &= help "Seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    , tezosHost =
        "localhost" &= help "Tezos host" &= explicit &=
        CmdArgs.name "tezos-host" &=
        CmdArgs.name "t" &=
        typ "DOMAIN"
    , tezosPort =
        18731 &= help "Port on Tezos host" &= explicit &=
        CmdArgs.name "tezos-port" &=
        CmdArgs.name "r" &=
        typ "PORT"
    , tezosPath =
        "localhost" &= help "Path on Tezos host" &= explicit &=
        CmdArgs.name "tezos-path" &=
        CmdArgs.name "h" &=
        typ "PATH"
    , secure = False &= help "Use HTTPS for Tezos requests"
    } &=
  help "Public wrapper API for a Tezos node." &=
  summary "tezos-client v0.1.0" &=
  program "tezos-client"

type Api = ()

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ServiceArgs T = Args
  type VariableSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = Api
  defaultArgs = _defaultArgs
  init Args {port, seedFile, tezosHost, tezosPort, tezosPath, secure} f = do
    (seed :: ByteString) <- ByteString.readFile $ unpack seedFile
    let (publicKey, secretKey) = seedKeyPair seed
    connection <-
      make
        Http.Config
          { schemeType =
              if secure
                then Http.HttpsType
                else Http.HttpType
          , host = Tagged tezosHost
          , port = Tagged tezosPort
          , path = Tagged tezosPath
          }
    with
      (WebSocket.localConfigOn port)
      (\webSocket -> f $ T {webSocket, publicKey, secretKey, connection})
