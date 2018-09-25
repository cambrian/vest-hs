module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import VestPrelude

class RpcTransport t where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
    -- ^ (publish -> res/Stream res -> IO ())
    -- Generic publisher on intermediate result x. Should encapsulate serializing the intermediate
    -- results.
    -> (Text -> Maybe req)
    -> Id "Rpc"
    -- ^ Should throw if route is already being served.
    -> t
    -> (req -> IO x)
    -> IO ()
  -- ^ Should mutate t to store the details necessary for cleanup.
  _call ::
       IO (res -> IO (), IO x, IO ())
    -- ^ IO (push, result, done)
    -- Generic response handler that takes an intermediate result x and defines how to push it to
    -- the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Id "Rpc"
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

-- Used to iterate over the nested API structure, run a (possibly monadic) function on the types of
-- each endpoint, and optionally collect the results in a list.
class Collector spec where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c) -> Proxy spec -> [c]
  -- ^ Function takes isStream -> (format, path, request type, response type).
  collectM ::
       (Monad m)
    => (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> m c)
    -> Proxy spec
    -> m [c]
  collectM fn = sequence . collect fn
  collectM_ ::
       (Monad m)
    => (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> m c)
    -> Proxy spec
    -> m ()
  collectM_ fn = void . collectM fn

instance (Collector a, Collector b) =>
         Collector (a
                    :<|> b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (a
              :<|> b)
    -> [c]
  collect fn _ = collect fn (Proxy :: Proxy a) ++ collect fn (Proxy :: Proxy b)

instance (Typeable f, Typeable s, Typeable a, Typeable b) =>
         Collector (DirectEndpointAs (f :: Format) (s :: Symbol) a b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (DirectEndpointAs f s a b)
    -> [c]
  collect fn _ =
    [ fn
        False
        ( typeRep (Proxy :: Proxy f)
        , typeRep (Proxy :: Proxy s)
        , typeRep (Proxy :: Proxy a)
        , typeRep (Proxy :: Proxy b))
    ]

instance (Typeable f, Typeable s, Typeable a, Typeable b) =>
         Collector (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) where
  collect ::
       (Bool -> (TypeRep, TypeRep, TypeRep, TypeRep) -> c)
    -> Proxy (StreamingEndpointAs f s a b)
    -> [c]
  collect fn _ =
    [ fn
        True
        ( typeRep (Proxy :: Proxy f)
        , typeRep (Proxy :: Proxy s)
        , typeRep (Proxy :: Proxy a)
        , typeRep (Proxy :: Proxy b))
    ]

data Streaming a
  = Result a
  | EndOfResults
  deriving (Read, Show, Generic, ToJSON, FromJSON)
