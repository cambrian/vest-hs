module Vest.Bridge.Rpc.Client
  ( module Vest.Bridge.Rpc.Client
  ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Stream
import Vest.Bridge.Rpc.Auth
import Vest.Bridge.Rpc.Prelude
import Vest.Prelude

type StreamCallback res a = (Stream res -> IO a) -> IO a

-- The structure of streaming RPC calls might seem more complicated than expected.
-- The reason for this is that the result stream may throw a HeartbeatLostException to the calling
-- thread at any time, so we limit them to within the body of the call by requiring the stream to be
-- consumed within a callback.
-- If you want to consume multiple streaming RPCs concurrently, you should nest the callbacks.
-- This is admittedly ugly but it handles exceptions correctly.
-- TODO: implement as monad transformer?
type family ClientBindings spec where
  ClientBindings () = ()
  ClientBindings (Endpoint_ _ _ _ _ _ _ req ('Direct res)) = req -> IO res
  ClientBindings (Endpoint_ _ _ _ _ _ _ req ('Streaming res)) = req -> (Stream res -> IO ()) -> IO ()
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

packRequest ::
     forall fmt signer req. (Serializable fmt req, RequestSigner signer)
  => signer
  -> req
  -> (Headers, Text' "Request")
packRequest signer req =
  let reqText = serialize' @fmt req
      headers = signRequest signer HashMap.empty reqText
   in (headers, reqText)

callDirect ::
     forall fmt signer transport req res.
     ( Serializable fmt req
     , Deserializable fmt (Either RpcClientException res)
     , RequestSigner signer
     , RpcTransport transport
     )
  => Time Second
  -> signer
  -> NamespacedText' "Route"
  -> transport
  -> req
  -> IO res
callDirect timeout_ signer route transport req = do
  resultVar <- newEmptyMVar
  mainThread <- myThreadId
  let (headersWithSignature, reqText) = packRequest @fmt signer req
      handleResponse resOrExcText =
        deserializeUnsafe' @fmt resOrExcText >>= \case
          Left (exc :: RpcClientException) -> evilThrowTo mainThread exc
          Right res -> putMVar resultVar res
  Tagged doCleanup <-
    _issueRequest handleResponse route transport headersWithSignature reqText
  result <- timeout timeout_ $ readMVar resultVar
  doCleanup
  throwIfTimeout result

callStreaming ::
     forall fmt signer transport req res a.
     ( Serializable fmt req
     , Deserializable fmt (Either RpcClientException (StreamingResponse res))
     , RequestSigner signer
     , RpcTransport transport
     )
  => Time Second
  -> signer
  -> NamespacedText' "Route"
  -> transport
  -> req
  -> (Stream res -> IO a)
  -> IO a
callStreaming timeout_ signer route transport req f = do
  (push, Tagged close, results) <- pushStream
  (renewHeartbeatTimer, heartbeatLostOrDone) <-
    timeoutRenewable
      (2 *:* timeoutsPerHeartbeat *:* timeout_)
      (Stream.mapM_ return results)
  gotFirstResponse <- newEmptyMVar
  mainThread <- myThreadId
  let (headersWithSignature, reqText) = packRequest @fmt signer req
      handleResponse resOrExcText = do
        _ <- tryPutMVar gotFirstResponse ()
        deserializeUnsafe' @fmt resOrExcText >>= \case
          Left (exc :: RpcClientException) -> evilThrowTo mainThread exc
          Right response -> do
            renewHeartbeatTimer (2 *:* timeoutsPerHeartbeat *:* timeout_)
            case response of
              Heartbeat -> return ()
              Result res -> push res
              EndOfResults -> close
  Tagged doCleanup <-
    _issueRequest handleResponse route transport headersWithSignature reqText
  timeout timeout_ (takeMVar gotFirstResponse) >>= \case
    Left exn -> throw exn
    Right () ->
      void . async $ do
        heartbeatLostOrDone_ <- heartbeatLostOrDone
        doCleanup
        case heartbeatLostOrDone_ of
          Left (TimeoutException time) ->
            evilThrowTo mainThread $ HeartbeatLostException time
          _ -> return ()
  f results

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (Either RpcClientException res)
         , HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt 'NoAuth server transport route req ('Direct res)) where
  makeClient ::
       t
    -> Proxy (Endpoint_ timeout fmt 'NoAuth server transport route req ('Direct res))
    -> (req -> IO res)
  makeClient t _ =
    callDirect
      @fmt
      (natSeconds @timeout)
      (authSigner @() t)
      (namespaced' @server $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (Either RpcClientException (StreamingResponse res))
         , HasNamespace server
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt 'NoAuth server transport route req ('Streaming res)) where
  makeClient ::
       t
    -> Proxy (Endpoint_ timeout fmt 'NoAuth server transport route req ('Streaming res))
    -> (req -> (Stream res -> IO ()) -> IO ())
  makeClient t _ =
    callStreaming
      @fmt
      (natSeconds @timeout)
      (authSigner @() t)
      (namespaced' @server $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (Either RpcClientException res)
         , HasNamespace server
         , HasAuthSigner auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Direct res)) where
  makeClient ::
       t
    -> Proxy (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Direct res))
    -> (req -> IO res)
  makeClient t _ =
    callDirect
      @fmt
      (natSeconds @timeout)
      (authSigner @auth t)
      (namespaced' @server $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (Either RpcClientException (StreamingResponse res))
         , HasNamespace server
         , HasAuthSigner auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Streaming res)) where
  makeClient ::
       t
    -> Proxy (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Streaming res))
    -> (req -> (Stream res -> IO ()) -> IO ())
  makeClient t _ =
    callStreaming
      @fmt
      (natSeconds @timeout)
      (authSigner @auth t)
      (namespaced' @server $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)
