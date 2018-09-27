module Manager
  ( ManagerApi
  , handlers
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified Bridge.Transports.WebSocket as WebSocket
import VestPrelude

data AddIntsSignedRequest = AddIntsSignedRequest
  { a :: Int
  , b :: Int
  , sign :: Bool
  }

newtype AddIntsSignedResponse =
  AddIntsSignedResponse Int

type AddIntsSignedEndpoint
   = Endpoint 'Direct 'NoAuth "addSignedInts" AddIntsSignedRequest AddIntsSignedResponse

type ManagerApi = AddIntsSignedEndpoint

addIntsSigned :: AddIntsSignedRequest -> IO AddIntsSignedResponse
addIntsSigned AddIntsSignedRequest {a, b, sign = True} =
  return $ AddIntsSignedResponse (a + b)
addIntsSigned AddIntsSignedRequest {a, b, sign = False} =
  return $ AddIntsSignedResponse $ -(a + b)

handlers :: Handlers ManagerApi
handlers = addIntsSigned
-- start :: WebSocket.T -> Amqp.T -> Amqp.T -> IO ()
-- start client
