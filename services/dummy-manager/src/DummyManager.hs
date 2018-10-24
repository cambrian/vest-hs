-- Pointless endpoints for WebSocket testing.
-- TODO: Test a single skipped heartbeat.
module DummyManager
  ( module DummyManager
  ) where

import DummyManager.Api as DummyManager
import qualified DummyManager.Auth as DummyAuth
import DummyManager.Internal as DummyManager
import qualified Stream
import qualified Transport.WebSocket as WebSocket
import Vest

data DummyManager = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

type Api
   = AddIntsEndpoint
     :<|> AddIntsBadEndpoint
     :<|> EchoThriceEndpoint
     :<|> EchoThriceBadEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint

addInts :: T -> AddIntsRequest -> IO Int
addInts _ AddIntsRequest {a, b} = do
  threadDelay (sec 0.25)
  return (a + b)

echoThrice :: T -> Int -> IO (Stream Int)
echoThrice _ x = do
  threadDelay (sec 0.25)
  return . Stream.fromList . replicate 3 $ x

concatTextAuth ::
     T
  -> VerifierClaims DummyAuth.T
  -> ConcatTextAuthRequest
  -> IO ConcatTextAuthResponse
concatTextAuth _ _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth :: T -> VerifierClaims DummyAuth.T -> Text -> IO (Stream Text)
echoThriceAuth _ _ = return . Stream.fromList . replicate 3

handlers :: Handlers Api
handlers =
  addInts :<|> addInts :<|> echoThrice :<|> echoThrice :<|> concatTextAuth :<|>
  echoThriceAuth

instance Service T where
  type ServiceArgs T = DummyManager
  type PublishSpec T = ()
  type RpcSpec T = Api
  defaultArgs = Args {}
  init _ f = with WebSocket.localConfig (\webSocket -> f $ T {webSocket})
