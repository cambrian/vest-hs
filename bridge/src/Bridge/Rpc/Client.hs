module Bridge.Rpc.Client
  ( module Bridge.Rpc.Client
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

data RpcClientException =
  BadCall Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family ClientBindings spec where
  ClientBindings (DirectEndpointAs (f :: Format) (s :: Symbol) a b) = Time Second -> a -> IO b
  ClientBindings (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) = Time Second -> a -> IO (Streamly.Serial b)
  ClientBindings (a
                  :<|> b) = (ClientBindings a
                             :<|> ClientBindings b)

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
      (Id $ proxyText (Proxy :: Proxy route))

instance (KnownSymbol route, Show req, Read res, RpcTransport transport) =>
         Client (StreamingEndpoint route req res) transport where
  makeClient ::
       Proxy (StreamingEndpoint route req res, transport)
    -> transport
    -> (Time Second -> req -> IO (Streamly.Serial res))
  makeClient _ =
    _call
      (do (_push, close, results) <- pushStream
          let push (Result res) = _push res
              push EndOfResults = close
              done = Streamly.mapM_ return results
          return (push, return results, done))
      show
      readUnsafe
      (Id $ proxyText (Proxy :: Proxy route))

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
      (Id $ proxyText (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON req, FromJSON res, RpcTransport transport) =>
         Client (StreamingEndpointJSON route req res) transport where
  makeClient ::
       Proxy (StreamingEndpointJSON route req res, transport)
    -> transport
    -> (Time Second -> req -> IO (Streamly.Serial res))
  makeClient _ =
    _call
      (do (_push, close, results) <- pushStream
          let push (Result res) = _push res
              push EndOfResults = close
              done = Streamly.mapM_ return results
          return (push, return results, done))
      encode
      decodeUnsafe
      (Id $ proxyText (Proxy :: Proxy route))
