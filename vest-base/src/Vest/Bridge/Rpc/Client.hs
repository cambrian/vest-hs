module Vest.Bridge.Rpc.Client
  ( module Vest.Bridge.Rpc.Client
  ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Stream
import Vest.Bridge.Rpc.Auth
import Vest.Bridge.Rpc.Prelude
import Vest.Prelude

type family ClientBindings spec where
  ClientBindings () = ()
  ClientBindings (Endpoint _ _ _ _ _ req ('Direct res)) = Time Second -> req -> IO res
  ClientBindings (Endpoint _ _ _ _ _ req ('Streaming res)) = Time Second -> req -> IO (Stream res)
  ClientBindings (a
                  :<|> b) = (ClientBindings a
                             :<|> ClientBindings b)

class (Auth a) =>
      HasAuthSigner a t
  where
  authSigner :: t -> AuthSigner a

-- | Empty auth signer is always defined.
instance HasAuthSigner () t where
  authSigner _ = ()

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

streamingPusher :: IO (ResultItem res -> IO (), IO (Stream res), IO' "Done" ())
streamingPusher = do
  (push_, Tagged close, results) <- pushStream
  let push (Result res) = push_ res
      push EndOfResults = close
      done = Stream.mapM_ return results
  return (push, return results, Tagged done)

_call ::
     forall fmt req res signer transport x.
     ( Serializable fmt req
     , Deserializable fmt (Either RpcClientException res)
     , RequestSigner signer
     , RpcTransport transport
     )
  => IO (res -> IO (), IO x, IO' "Done" ())
  -> signer
  -> NamespacedText' "Route"
  -> transport
  -> Time Second
  -> req
  -> IO x
_call pusher signer route transport timeout req = do
  (push, result, Tagged done) <- pusher
  (Tagged renewTimeout, timeoutOrDone) <- timeoutRenewable timeout done
  let headers = HashMap.empty
      reqText = serialize' @fmt req
      headersWithSignature = signRequest signer headers reqText
      handleResponse resOrExcText = do
        deserializeUnsafe' @fmt resOrExcText >>= \case
          Left exc -> throw (exc :: RpcClientException)
          Right res -> push res
        renewTimeout
  Tagged doCleanup <-
    _issueRequest handleResponse route transport headersWithSignature reqText
  mainThread <- myThreadId
  void . async $ do
    timeoutOrDone_ <- timeoutOrDone
    doCleanup
    case timeoutOrDone_ of
      Left exn -> evilThrowTo mainThread exn -- TODO: Is this bad?
      _ -> return ()
  result

instance ( Serializable fmt req
         , Deserializable fmt (Either RpcClientException res)
         , HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint fmt 'NoAuth server transport route req ('Direct res)) where
  makeClient ::
       t
    -> Proxy (Endpoint fmt 'NoAuth server transport route req ('Direct res))
    -> (Time Second -> req -> IO res)
  makeClient t _ =
    _call
      @fmt
      directPusher
      (authSigner @() t)
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( Serializable fmt req
         , Deserializable fmt (Either RpcClientException (ResultItem res))
         , HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint fmt 'NoAuth server transport route req ('Streaming res)) where
  makeClient ::
       t
    -> Proxy (Endpoint fmt 'NoAuth server transport route req ('Streaming res))
    -> (Time Second -> req -> IO (Stream res))
  makeClient t _ =
    _call
      @fmt
      streamingPusher
      (authSigner @() t)
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( Serializable fmt req
         , Deserializable fmt (Either RpcClientException res)
         , HasNamespace server
         , HasAuthSigner auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint fmt ('Auth auth) server transport route req ('Direct res)) where
  makeClient ::
       t
    -> Proxy (Endpoint fmt ('Auth auth) server transport route req ('Direct res))
    -> (Time Second -> req -> IO res)
  makeClient t _ =
    _call
      @fmt
      directPusher
      (authSigner @auth t)
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( Serializable fmt req
         , Deserializable fmt (Either RpcClientException (ResultItem res))
         , HasNamespace server
         , HasAuthSigner auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint fmt ('Auth auth) server transport route req ('Streaming res)) where
  makeClient ::
       t
    -> Proxy (Endpoint fmt ('Auth auth) server transport route req ('Streaming res))
    -> (Time Second -> req -> IO (Stream res))
  makeClient t _ =
    _call
      @fmt
      streamingPusher
      (authSigner @auth t)
      (namespaced' @server $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)
