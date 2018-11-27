module DummyManager.Api
  ( module DummyManager.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import DummyManager.Internal as DummyManager
import qualified Transport.WebSocket as WebSocket
import Vest

data AddIntsRequest = AddIntsRequest
  { a :: Int
  , b :: Int
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''AddIntsRequest)

type AddIntsEndpoint
   = EndpointJson 'NoAuth DummyManager.T WebSocket.T "addInts" AddIntsRequest ('Direct Int)

type AddIntsBadEndpoint
   = Endpoint_ 0 'JSON 'NoAuth DummyManager.T WebSocket.T "addIntsBad" AddIntsRequest ('Direct Int)

type EchoThriceEndpoint
   = EndpointJson 'NoAuth DummyManager.T WebSocket.T "echoThrice" Int ('Streaming Int)

type EchoThriceBadEndpoint
   = Endpoint_ 0 'JSON 'NoAuth DummyManager.T WebSocket.T "echoThriceBad" Int ('Streaming Int)

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
   = EndpointJson ('Auth ()) DummyManager.T WebSocket.T "concatTextAuth" ConcatTextAuthRequest ('Direct ConcatTextAuthResponse)

type EchoThriceAuthEndpoint
   = EndpointJson ('Auth ()) DummyManager.T WebSocket.T "echoThriceAuth" Text ('Streaming Text)

type VoidEndpoint
   = EndpointJson 'NoAuth DummyManager.T WebSocket.T "getVoid" () ('Direct ())

type VoidStreamEndpoint
   = EndpointJson 'NoAuth DummyManager.T WebSocket.T "getVoidStream" () ('Streaming ())
