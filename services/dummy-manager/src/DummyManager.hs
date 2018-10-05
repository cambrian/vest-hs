-- Pointless endpoints for WebSocket testing.
module DummyManager
  ( module DummyManager
  ) where

import Bridge
import qualified Bridge.Rpc.Auth.Token as Token
import qualified Bridge.Transports.WebSocket as WebSocket
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

data DummyManager = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { webSocket :: WebSocket.T
  }

instance Service T where
  type ServiceArgs T = DummyManager
  defaultArgs = Args {}
  run _args f = with WebSocket.localConfig (\webSocket -> f $ T {webSocket})

data AddIntsRequest = AddIntsRequest
  { a :: Int
  , b :: Int
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''AddIntsRequest)

type AddIntsEndpoint = Endpoint T 'NoAuth "addInts" AddIntsRequest ('Direct Int)

type EchoThriceEndpoint = Endpoint T 'NoAuth "echoThrice" Int ('Streaming Int)

data ConcatTextAuthRequest = ConcatTextAuthRequest
  { a :: Text
  , b :: Text
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ConcatTextAuthRequest)

data ConcatTextAuthResponse = ConcatTextAuthResponse
  { result :: Text
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ConcatTextAuthResponse)

type ConcatTextAuthEndpoint
   = Endpoint T ('Auth Token.T) "concatTextAuth" ConcatTextAuthRequest ('Direct ConcatTextAuthResponse)

type EchoThriceAuthEndpoint
   = Endpoint T ('Auth Token.T) "echoThriceAuth" Text ('Streaming Text)

type Api
   = AddIntsEndpoint
     :<|> EchoThriceEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint

addInts :: T -> AddIntsRequest -> IO Int
addInts _ AddIntsRequest {a, b} = do
  threadDelay (sec 0.25)
  return (a + b)

echoThrice :: T -> Int -> IO (Streamly.Serial Int)
echoThrice _ x = do
  threadDelay (sec 0.25)
  return . Streamly.fromList . replicate 3 $ x

concatTextAuth ::
     T -> Claims Token.T -> ConcatTextAuthRequest -> IO ConcatTextAuthResponse
concatTextAuth _ _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth :: T -> Claims Token.T -> Text -> IO (Streamly.Serial Text)
echoThriceAuth _ _ = return . Streamly.fromList . replicate 3

handlers :: Handlers Api
handlers = addInts :<|> echoThrice :<|> concatTextAuth :<|> echoThriceAuth
