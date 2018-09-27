module Manager
  ( ManagerApi
  , start
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified Bridge.Transports.WebSocket as WebSocket
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import VestPrelude

data AddIntsSignedRequest = AddIntsSignedRequest
  { a :: Int
  , b :: Int
  , sign :: Bool
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''AddIntsSignedRequest)

type AddIntsSignedEndpoint
   = Endpoint 'Direct 'NoAuth "addSignedInts" AddIntsSignedRequest Int

type ManagerApi = AddIntsSignedEndpoint

addIntsSigned :: AddIntsSignedRequest -> IO Int
addIntsSigned AddIntsSignedRequest {a, b, sign = True} = return $ (a + b)
addIntsSigned AddIntsSignedRequest {a, b, sign = False} = return $ -(a + b)

handlers :: Handlers ManagerApi
handlers = addIntsSigned

start :: WebSocket.T -> Amqp.T -> Amqp.T -> IO ()
start serverTransport _ _ =
  serve (Proxy :: Proxy (ManagerApi, WebSocket.T)) handlers serverTransport
