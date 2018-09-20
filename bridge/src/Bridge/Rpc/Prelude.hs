module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import VestPrelude

class RpcTransport t where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
    -- ^ (publish -> res/Stream res -> IO ())
    -- Generic publisher on intermediate result x. Should
    -- encapsulate serializing the intermediate results.
    -> (Text -> Maybe req)
    -> Route
    -- ^ Should throw if Route is already being served.
    -> t
    -> (req -> IO x)
    -> IO ()
  -- ^ Should mutate t to store the details necessary for cleanup.
  _call ::
       IO (res -> IO (), IO x, IO ())
    -- ^ IO (push, result, done)
    -- Generic response handler that takes an intermediate result x and defines how to push it
    -- to the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Route
    -> t
    -> Time Second
    -- ^ timeout
    -> req
    -> IO x
  -- ^ Registers a handler in the waiting RPC call hash table, and deregisters it after done
  -- resolves.
  -- Timeouts occur if a result or stream result is not received after the specified time.

data DirectEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type DirectEndpoint = DirectEndpointAs 'Haskell

type DirectEndpointJSON = DirectEndpointAs 'JSON

data StreamingEndpointAs (f :: Format) (s :: k) (a :: *) (b :: *)

type StreamingEndpoint = StreamingEndpointAs 'Haskell

type StreamingEndpointJSON = StreamingEndpointAs 'JSON

data Streaming a
  = Result a
  | EndOfResults
  deriving (Read, Show, Generic, ToJSON, FromJSON)
