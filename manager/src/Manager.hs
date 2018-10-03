-- Right now this file is just pointless endpoints for WebSocket testing.
module Manager
  ( ManagerApi
  , start
  ) where

-- import qualified Bridge.Transports.Amqp as Amqp
import Bridge
import qualified Bridge.Transports.WebSocket as WebSocket
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

data AddIntsRequest = AddIntsRequest
  { a :: Int
  , b :: Int
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''AddIntsRequest)

type AddIntsEndpoint = Endpoint 'NoAuth "addInts" AddIntsRequest ('Direct Int)

type EchoThriceEndpoint = Endpoint 'NoAuth "echoThrice" Int ('Streaming Int)

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
   = Endpoint ('Auth TokenAuth) "concatTextAuth" ConcatTextAuthRequest ('Direct ConcatTextAuthResponse)

type EchoThriceAuthEndpoint
   = Endpoint ('Auth TokenAuth) "echoThriceAuth" Text ('Streaming Text)

type ManagerApi
   = AddIntsEndpoint
     :<|> EchoThriceEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint

addInts :: AddIntsRequest -> IO Int
addInts AddIntsRequest {a, b} = do
  threadDelay (sec 0.25)
  return (a + b)

echoThrice :: Int -> IO (Streamly.Serial Int)
echoThrice x = do
  threadDelay (sec 0.25)
  return . Streamly.fromList . replicate 3 $ x

concatTextAuth ::
     Claims TokenAuth -> ConcatTextAuthRequest -> IO ConcatTextAuthResponse
concatTextAuth _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth :: Claims TokenAuth -> Text -> IO (Streamly.Serial Text)
echoThriceAuth _ = return . Streamly.fromList . replicate 3

handlers :: Handlers ManagerApi
handlers = addInts :<|> echoThrice :<|> concatTextAuth :<|> echoThriceAuth

start :: WebSocket.T -> WebSocket.T -> WebSocket.T -> IO ()
start serverTransport _ _ =
  serve handlers (Proxy :: Proxy (ManagerApi, WebSocket.T)) serverTransport
