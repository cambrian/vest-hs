module Butler.Rpc.Prelude
  ( module Butler.Rpc.Prelude
  ) where

import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

class RpcTransport t where
  _serve ::
       ((Text -> IO ()) -> x -> IO ()) -- (publish -> res/Stream res -> IO ())
    -> (Text -> Maybe req)
    -> Route
    -> t
    -> (req -> IO x)
    -> IO () -- should mutate t to store the details necessary for kill
  -- handler :: IO (responseHandler, result, done) defines how to wrap results up for the caller.
  -- Registers a handler in the waiting RPC call hash table, and deregisters it after done resolves.
  -- Times out if done is not fulfilled after _timeout, with the timeout reset every time a response
  -- is received.
  _call ::
       IO (res -> IO (), IO x, IO ()) -- push, result, done
    -> (req -> Text)
    -> (Text -> IO res)
    -> Route
    -> t
    -> Time Second -- timeout
    -> req
    -> IO x

data DirectEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type DirectEndpoint = DirectEndpointAs 'Haskell

type DirectEndpointJSON = DirectEndpointAs 'JSON

data StreamingEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type StreamingEndpoint = StreamingEndpointAs 'Haskell

type StreamingEndpointJSON = StreamingEndpointAs 'JSON

data RpcException =
  BadCall Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

data Streaming a
  = Result a
  | EndOfResults
  deriving (Read, Show, Generic, ToJSON, FromJSON)
