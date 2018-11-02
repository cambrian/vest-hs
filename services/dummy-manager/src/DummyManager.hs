-- Pointless endpoints for WebSocket testing.
-- TODO: Test a single skipped heartbeat.
module DummyManager
  ( module DummyManager
  ) where

import DummyManager.Api as DummyManager
import qualified DummyManager.Auth as DummyAuth
import DummyManager.Internal as DummyManager
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Args = Args
  { port :: Word16
  } deriving (Eq, Show, Read, Generic, Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { port =
        3000 &= help "Port to serve on" &= CmdArgs.name "port" &=
        CmdArgs.name "p" &=
        typ "PORT"
    } &=
  help "Dummy manager service." &=
  summary "dummy-manager v0.1.0" &=
  program "dummy-manager"

type Api
   = AddIntsEndpoint
     :<|> AddIntsBadEndpoint
     :<|> EchoThriceEndpoint
     :<|> EchoThriceBadEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint
     :<|> VoidEndpoint
     :<|> VoidStreamEndpoint

addInts :: T -> AddIntsRequest -> IO Int
addInts _ AddIntsRequest {a, b} = do
  threadDelay (ms 30)
  return (a + b)

echoThrice :: T -> Int -> IO (Stream ValueBuffer Int)
echoThrice _ x = do
  (pusher, stream) <- newStream
  async $
    mapM_ (\n -> threadDelay (ms 30) >> writeStream pusher n) [x .. x + 2] >>
    closeStream pusher
  return stream

concatTextAuth ::
     T
  -> VerifierClaims DummyAuth.T
  -> ConcatTextAuthRequest
  -> IO ConcatTextAuthResponse
concatTextAuth _ _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth ::
     T -> VerifierClaims DummyAuth.T -> Text -> IO (Stream ValueBuffer Text)
echoThriceAuth _ _ = streamFromList . replicate 3

unit :: T -> () -> IO ()
unit _ _ = return ()

unitStream :: T -> () -> IO (Stream ValueBuffer ())
unitStream _ _ = streamFromList $ replicate 3 ()

handlers :: Handlers Api
handlers =
  addInts :<|> addInts :<|> echoThrice :<|> echoThrice :<|> concatTextAuth :<|>
  echoThriceAuth :<|>
  unit :<|>
  unitStream

instance Service T where
  type ServiceArgs T = Args
  type RpcSpec T = Api
  type ValueSpec T = ()
  type EventSpec T = ()
  defaultArgs = defaultArgs_
  init Args {port} f =
    with (WebSocket.localConfigOn port) (\webSocket -> f $ T {webSocket})
