module Bridge.Rpc.Client
  ( module Bridge.Rpc.Client
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family ClientBindings spec where
  ClientBindings (Endpoint 'Direct _ _ req res) = Time Second -> Headers -> req -> IO res
  ClientBindings (Endpoint 'Streaming _ _ req res) = Time Second -> Headers -> req -> IO (Streamly.Serial res)
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

directPusher :: IO (res -> IO (), IO res, IO ())
directPusher = do
  resultVar <- newEmptyMVar
  let push = putMVar resultVar
      result = readMVar resultVar
      done = void result
  return (push, result, done)

streamingPusher :: IO (ResultItem res -> IO (), IO (Streamly.Serial res), IO ())
streamingPusher = do
  (push_, close, results) <- pushStream
  let push (Result res) = push_ res
      push EndOfResults = close
      done = Streamly.mapM_ return results
  return (push, return results, done)

_call ::
     (Show req, ToJSON req, Read res, FromJSON res, RpcTransport transport)
  => IO (res -> IO (), IO x, IO ())
  -> Text' "Route"
  -> transport
  -> Time Second
  -> Headers
  -> req
  -> IO x
_call pusher route transport timeout_ headers req = do
    (push, result, done) <- pusher
    (renewTimeout, timeoutOrDone) <- timeoutRenewable timeout_ done
    let fmt = format headers
        (serialize_, deserializeUnsafe_) = runtimeSerializationsOf' $ fmt
        handleResponse resOrExcText = do
          deserializeUnsafe_ resOrExcText >>= \case
            Left exc -> throw (exc :: RpcClientException)
            Right res -> push res
          renewTimeout
    cleanup <- _issueRequest handleResponse route transport headers (serialize_ req)
    mainThread <- myThreadId
    void . async $ do
      timeoutOrDone_ <- timeoutOrDone
      cleanup
      case timeoutOrDone_ of
        Left exn -> evilThrowTo mainThread exn -- TODO: We should probably change this?
        _ -> return ()
    result

instance ( KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Direct h (route :: Symbol) req res) transport where
  makeClient ::
       Proxy (Endpoint 'Direct h route req res, transport)
    -> transport
    -> (Time Second -> Headers -> req -> IO res)
  makeClient _ = _call directPusher (proxyText' (Proxy :: Proxy route))

instance ( KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Streaming h (route :: Symbol) req res) transport where
  makeClient ::
       Proxy (Endpoint 'Streaming h route req res, transport)
    -> transport
    -> (Time Second -> Headers -> req -> IO (Streamly.Serial res))
  makeClient _ = _call streamingPusher (proxyText' (Proxy :: Proxy route))
