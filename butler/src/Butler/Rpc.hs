module Butler.Rpc
  ( module Butler.Rpc
  ) where

import Butler.Dsl
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

data Streaming a
  = Result a
  | EndOfResults
  deriving (Read, Show, Generic, ToJSON, FromJSON)

data ButlerException =
  BadCall Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

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

type family Handlers spec :: *

type instance
     Handlers (DirectEndpointAs (f :: Format) (s :: Symbol) a b) =
     a -> IO b

type instance
     Handlers (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) =
     a -> IO (Streamly.Serial b)

type instance Handlers (a :<|> b) = Handlers a :<|> Handlers b

class (RpcTransport transport) =>
      Server spec transport
  where
  serve :: Proxy (spec, transport) -> transport -> Handlers spec -> IO ()

instance (Server a transport, Server b transport) =>
         Server (a
                 :<|> b) transport where
  serve ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> (Handlers a
        :<|> Handlers b)
    -> IO ()
  serve _ transport (aHandlers :<|> bHandlers) = do
    serve (Proxy :: Proxy (a, transport)) transport aHandlers
    serve (Proxy :: Proxy (b, transport)) transport bHandlers

instance (KnownSymbol s, Read a, Show b, RpcTransport transport) =>
         Server (DirectEndpoint (s :: Symbol) a b) transport where
  serve ::
       Proxy (DirectEndpoint s a b, transport)
    -> transport
    -> (a -> IO b)
    -> IO ()
  serve _ = _serve (. show) read (Route $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, Read a, Show b, RpcTransport transport) =>
         Server (StreamingEndpoint (s :: Symbol) a b) transport where
  serve ::
       Proxy (StreamingEndpoint s a b, transport)
    -> transport
    -> (a -> IO (Streamly.Serial b))
    -> IO ()
  serve _ =
    _serve
      (\pub xs -> do
         Streamly.mapM_ (pub . show . Result) xs
         pub $ show $ EndOfResults @b)
      read
      (Route $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, FromJSON a, ToJSON b, RpcTransport transport) =>
         Server (DirectEndpointJSON (s :: Symbol) a b) transport where
  serve ::
       Proxy (DirectEndpointJSON s a b, transport)
    -> transport
    -> (a -> IO b)
    -> IO ()
  serve _ = _serve (. encode) decode (Route . proxyText $ (Proxy :: Proxy s))

instance (KnownSymbol s, FromJSON a, ToJSON b, RpcTransport transport) =>
         Server (StreamingEndpointJSON (s :: Symbol) a b) transport where
  serve ::
       Proxy (StreamingEndpointJSON s a b, transport)
    -> transport
    -> (a -> IO (Streamly.Serial b))
    -> IO ()
  serve _ =
    _serve
      (\pub xs -> do
         Streamly.mapM_ (pub . encode . Result) xs
         pub . encode $ EndOfResults @b)
      decode
      (Route $ proxyText (Proxy :: Proxy s))

type family ClientBindings spec :: *

type instance
     ClientBindings (DirectEndpointAs (f :: Format) (s :: Symbol) a b) =
     Time Second -> a -> IO b

type instance
     ClientBindings
       (StreamingEndpointAs (f :: Format) (s :: Symbol) a b)
     = Time Second -> a -> IO (Streamly.Serial b)

type instance ClientBindings (a :<|> b) =
     ClientBindings a :<|> ClientBindings b

class (RpcTransport transport) =>
      Client spec transport
  where
  makeClient :: Proxy (spec, transport) -> transport -> ClientBindings spec

instance (Client a transport, Client b transport) =>
         Client (a
                 :<|> b) transport where
  makeClient ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> ClientBindings a
       :<|> ClientBindings b
  makeClient _ transport =
    makeClient (Proxy :: Proxy (a, transport)) transport :<|>
    makeClient (Proxy :: Proxy (b, transport)) transport

instance (KnownSymbol route, Show req, Read res, RpcTransport transport) =>
         Client (DirectEndpoint route req res) transport where
  makeClient ::
       Proxy (DirectEndpoint route req res, transport)
    -> transport
    -> (Time Second -> req -> IO res)
  makeClient _ =
    _call
      (do resultVar <- newEmptyMVar
          let push = putMVar resultVar
              result = readMVar resultVar
              done = void result
          return (push, result, done))
      show
      readUnsafe
      (Route $ proxyText (Proxy :: Proxy route))

instance (KnownSymbol route, Show req, Read res, RpcTransport transport) =>
         Client (StreamingEndpoint route req res) transport where
  makeClient ::
       Proxy (StreamingEndpoint route req res, transport)
    -> transport
    -> (Time Second -> req -> IO (Streamly.Serial res))
  makeClient _ =
    _call
      (do (_push, close, results) <- repeatableStream
          let push (Result res) = _push res
              push EndOfResults = close
              done = Streamly.mapM_ return results
          return (push, return results, done))
      show
      readUnsafe
      (Route $ proxyText (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON req, FromJSON res, RpcTransport transport) =>
         Client (DirectEndpointJSON route req res) transport where
  makeClient ::
       Proxy (DirectEndpointJSON route req res, transport)
    -> transport
    -> (Time Second -> req -> IO res)
  makeClient _ =
    _call
      (do resultVar <- newEmptyMVar
          let push = putMVar resultVar
              result = readMVar resultVar
              done = void result
          return (push, result, done))
      encode
      decodeUnsafe
      (Route $ proxyText (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON req, FromJSON res, RpcTransport transport) =>
         Client (StreamingEndpointJSON route req res) transport where
  makeClient ::
       Proxy (StreamingEndpointJSON route req res, transport)
    -> transport
    -> (Time Second -> req -> IO (Streamly.Serial res))
  makeClient _ =
    _call
      (do (_push, close, results) <- repeatableStream
          let push (Result res) = _push res
              push EndOfResults = close
              done = Streamly.mapM_ return results
          return (push, return results, done))
      encode
      decodeUnsafe
      (Route $ proxyText (Proxy :: Proxy route))
