module Bridge.Rpc.Client
  ( module Bridge.Rpc.Client
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest.Prelude

type family ClientBindings spec where
  ClientBindings () = ()
  ClientBindings (Endpoint _ _ _ _ req ('Direct res)) = Time Second -> Headers -> req -> IO res
  ClientBindings (Endpoint _ _ _ _ req ('Streaming res)) = Time Second -> Headers -> req -> IO (Streamly.Serial res)
  ClientBindings (a
                  :<|> b) = (ClientBindings a
                             :<|> ClientBindings b)

class Client t spec where
  makeClient :: t -> Proxy spec -> ClientBindings spec

instance Client t () where
  makeClient _ _ = ()

instance (Client t a, Client t b) =>
         Client t (a
                   :<|> b) where
  makeClient ::
       t
    -> Proxy (a
              :<|> b)
    -> ClientBindings a
       :<|> ClientBindings b
  makeClient t _ =
    makeClient t (Proxy :: Proxy a) :<|> makeClient t (Proxy :: Proxy b)

directPusher :: IO (res -> IO (), IO res, IO' "Done" ())
directPusher = do
  resultVar <- newEmptyMVar
  let push = putMVar resultVar
      result = readMVar resultVar
      done = void result
  return (push, result, Tagged done)

streamingPusher ::
     IO (ResultItem res -> IO (), IO (Streamly.Serial res), IO' "Done" ())
streamingPusher = do
  (push_, Tagged close, results) <- pushStream
  let push (Result res) = push_ res
      push EndOfResults = close
      done = Streamly.mapM_ return results
  return (push, return results, Tagged done)

_call ::
     (Show req, ToJSON req, Read res, FromJSON res, RpcTransport transport)
  => IO (res -> IO (), IO x, IO' "Done" ())
  -> NamespacedText' "Route"
  -> transport
  -> Time Second
  -> Headers
  -> req
  -> IO x
_call pusher route transport timeout_ headers req = do
  (push, result, Tagged done) <- pusher
  (Tagged renewTimeout, timeoutOrDone) <- timeoutRenewable timeout_ done
  let fmt = format headers
      (serialize_, deserializeUnsafe_) = runtimeSerializationsOf' fmt
      handleResponse resOrExcText = do
        deserializeUnsafe_ resOrExcText >>= \case
          Left exc -> throw (exc :: RpcClientException)
          Right res -> push res
        renewTimeout
  Tagged doCleanup <-
    _issueRequest handleResponse route transport headers (serialize_ req)
  mainThread <- myThreadId
  void . async $ do
    timeoutOrDone_ <- timeoutOrDone
    doCleanup
    case timeoutOrDone_ of
      Left exn -> evilThrowTo mainThread exn -- TODO: Is this bad?
      _ -> return ()
  result

instance ( HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         ) =>
         Client t (Endpoint server _auth transport route req ('Direct res)) where
  makeClient ::
       t
    -> Proxy (Endpoint server _auth transport route req ('Direct res))
    -> (Time Second -> Headers -> req -> IO res)
  makeClient t _ =
    _call
      directPusher
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         ) =>
         Client t (Endpoint server _auth transport route req ('Streaming res)) where
  makeClient ::
       t
    -> Proxy (Endpoint server _auth transport route req ('Streaming res))
    -> (Time Second -> Headers -> req -> IO (Streamly.Serial res))
  makeClient t _ =
    _call
      streamingPusher
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)
