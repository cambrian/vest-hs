-- Right now this file is just pointless endpoints for WebSocket testing.
module Manager
  ( ManagerApi
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
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

type AddIntsEndpoint = Endpoint 'Direct 'NoAuth "addInts" AddIntsRequest Int

type EchoThriceEndpoint = Endpoint 'Streaming 'NoAuth "echoThrice" Int Int

data ConcatTextAuthRequest = ConcatTextAuthRequest
  { a :: Text
  , b :: Text
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ConcatTextAuthRequest)

type ConcatTextAuthEndpoint
   = Endpoint 'Direct ('Auth TokenAuth) "concatTextAuth" ConcatTextAuthRequest Text

type EchoThriceAuthEndpoint
   = Endpoint 'Streaming ('Auth TokenAuth) "echoThriceAuth" Text Text

type ManagerApi
   = AddIntsEndpoint
     :<|> EchoThriceEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint

addInts :: AddIntsRequest -> IO Int
addInts AddIntsRequest {a, b} = return $ (a + b)

echoThrice :: Int -> IO (Streamly.Serial Int)
echoThrice = return . Streamly.fromList . replicate 3

concatTextAuth :: Claims TokenAuth -> ConcatTextAuthRequest -> IO Text
concatTextAuth _ ConcatTextAuthRequest {a, b} = return $ a <> b

echoThriceAuth :: Claims TokenAuth -> Text -> IO (Streamly.Serial Text)
echoThriceAuth _ = return . Streamly.fromList . replicate 3

handlers :: Handlers ManagerApi
handlers = addInts :<|> echoThrice :<|> concatTextAuth :<|> echoThriceAuth

start :: WebSocket.T -> Amqp.T -> Amqp.T -> IO ()
start serverTransport _ _ =
  serve (Proxy :: Proxy (ManagerApi, WebSocket.T)) handlers serverTransport
