-- Pointless endpoints for WebSocket testing.
module DummyManager
  ( module DummyManager
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Transports.WebSocket as WebSocket
import Vest

data DummyAuth =
  DummyAuth

instance RequestVerifier DummyAuth where
  type VerifierClaims DummyAuth = ()
  verifyRequest _ _ _ _ = Just ()

instance RequestSigner DummyAuth where
  signRequest _ headers _ = headers

instance Auth DummyAuth where
  type AuthVerifier DummyAuth = DummyAuth
  type AuthSigner DummyAuth = DummyAuth

data DummyManager = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { dummyAuth :: DummyAuth
  , webSocket :: WebSocket.T
  }

instance HasAuthVerifier DummyAuth T where
  authVerifier = dummyAuth

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

data AddIntsRequest = AddIntsRequest
  { a :: Int
  , b :: Int
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''AddIntsRequest)

type AddIntsEndpoint
   = Endpoint "JSON" 'NoAuth T WebSocket.T "addInts" AddIntsRequest ('Direct Int)

type EchoThriceEndpoint
   = Endpoint "JSON" 'NoAuth T WebSocket.T "echoThrice" Int ('Streaming Int)

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
   = Endpoint "JSON" ('Auth DummyAuth) T WebSocket.T "concatTextAuth" ConcatTextAuthRequest ('Direct ConcatTextAuthResponse)

type EchoThriceAuthEndpoint
   = Endpoint "JSON" ('Auth DummyAuth) T WebSocket.T "echoThriceAuth" Text ('Streaming Text)

type Api
   = AddIntsEndpoint
     :<|> EchoThriceEndpoint
     :<|> ConcatTextAuthEndpoint
     :<|> EchoThriceAuthEndpoint

instance Service T where
  type ServiceArgs T = DummyManager
  type PubSubSpec T = ()
  type RpcSpec T = Api
  defaultArgs = Args {}
  init _ f =
    with
      WebSocket.localConfig
      (\webSocket -> f $ T {dummyAuth = DummyAuth, webSocket})
