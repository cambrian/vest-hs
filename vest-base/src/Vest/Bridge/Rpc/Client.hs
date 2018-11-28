module Vest.Bridge.Rpc.Client
  ( Client
  , ClientBindings
  , makeClient
  ) where

import qualified Data.HashMap.Strict as HashMap
import Vest.Bridge.Rpc.Auth
import Vest.Bridge.Rpc.Prelude
import Vest.Prelude

-- The structure of streaming RPC calls might seem more complicated than expected.
-- The reason for this is that the result stream may throw a HeartbeatLostException to the calling
-- thread at any time, so we limit them to within the body of the call by requiring the stream to be
-- consumed within a callback.
-- If you want to consume multiple streaming RPCs concurrently, you should nest the callbacks.
-- This is admittedly ugly but it handles exceptions correctly.
--
-- Note: we would prefer req -> (Stream res -> IO a) -> IO a, but the free variable a is disallowed
-- by the type family declaration. Can we get around this?
type family ClientBindings spec where
  ClientBindings () = ()
  ClientBindings (Endpoint_ _ _ _ _ _ _ req ('Direct res)) = req -> IO res
  ClientBindings (Endpoint_ _ _ _ _ _ _ req ('Streaming res)) = req -> (Stream ValueBuffer res -> IO ()) -> IO ()
  ClientBindings (a
                  :<|> b) = (ClientBindings a
                             :<|> ClientBindings b)

newtype ClientException =
  ClientException Text
  deriving (Eq, Show)
  deriving anyclass (Exception)

data ServerException =
  ServerException
  deriving (Eq, Show)
  deriving anyclass (Exception)

class Client t spec where
  makeClient :: t -> Proxy spec -> ClientBindings spec

instance Client t () where
  makeClient _ _ = ()

instance (Client t a, Client t b) =>
         Client t (a
                   :<|> b) where
  makeClient t _ =
    makeClient t (Proxy :: Proxy a) :<|> makeClient t (Proxy :: Proxy b)

packRequest ::
     forall fmt auth req. Serializable fmt req
  => AuthSigner auth
  -> req
  -> IO (Headers, Text' "Request")
packRequest signer req = do
  let reqText = serialize' @fmt req
  headers <- atomically $ signRequest signer reqText HashMap.empty
  return (headers, reqText)

callDirect ::
     forall timeout fmt auth transport server route t req res.
     ( KnownNat timeout
     , Serializable fmt req
     , Deserializable fmt (RpcResponse res)
     , HasNamespace server
     , Has (AuthSigner auth) t
     , RpcTransport transport
     , Has transport t
     , KnownSymbol route
     )
  => t
  -> req
  -> IO res
callDirect t req = do
  resultVar <- newEmptyMVar
  mainThread <- myThreadId
  (headersWithSignature, reqText) <-
    packRequest @fmt (get @(AuthSigner auth) t) req
  let rawRoute = serialize' @'Pretty $ Namespaced @server (Proxy :: Proxy route)
      handleResponse resOrExcText =
        deserializeUnsafe' @fmt resOrExcText >>= \case
          RpcResponseClientException eText ->
            evilThrowTo mainThread $ ClientException eText
          RpcResponseServerException -> evilThrowTo mainThread ServerException
          RpcResponse res -> putMVar resultVar res
  Tagged doCleanup <-
    callRaw
      (get @transport t)
      rawRoute
      headersWithSignature
      reqText
      handleResponse
  result <- timeout (natSeconds @timeout) (readMVar resultVar)
  doCleanup
  throwIfTimeout result

callStreaming ::
     forall timeout fmt auth transport server route t req res a.
     ( KnownNat timeout
     , Serializable fmt req
     , Deserializable fmt (RpcResponse (StreamingResponse res))
     , HasNamespace server
     , Has (AuthSigner auth) t
     , RpcTransport transport
     , Has transport t
     , KnownSymbol route
     , Eq res
     )
  => t
  -> req
  -> (Stream ValueBuffer res -> IO a)
  -> IO a
callStreaming t req f = do
  let timeout_ = natSeconds @timeout
      twoHeartbeats = 2 *^ timeoutsPerHeartbeat *^ timeout_
      rawRoute = serialize' @'Pretty $ Namespaced @server (Proxy :: Proxy route)
  (resultPusher, results) <- newStream
  (renewHeartbeatTimer, heartbeatLostOrDone) <-
    timeoutRenewable twoHeartbeats $ waitStream results
  gotFirstResponse <- newEmptyMVar
  mainThread <- myThreadId
  (headersWithSignature, reqText) <-
    packRequest @fmt (get @(AuthSigner auth) t) req
  let handleResponse resOrExcText = do
        void $ tryPutMVar gotFirstResponse ()
        renewHeartbeatTimer twoHeartbeats
        deserializeUnsafe' @fmt resOrExcText >>= \case
          RpcResponseClientException eText ->
            evilThrowTo mainThread $ ClientException eText
          RpcResponseServerException -> evilThrowTo mainThread ServerException
            -- does this cause doCleanup to get skipped?
          RpcResponse response ->
            case response of
              Heartbeat -> return ()
              Result res -> writeStream resultPusher res
              EndOfResults -> closeStream resultPusher
  Tagged doCleanup <-
    callRaw
      (get @transport t)
      rawRoute
      headersWithSignature
      reqText
      handleResponse
  timeout timeout_ (takeMVar gotFirstResponse) >>= \case
    Left exn -> do
      closeStream resultPusher
      doCleanup
      throw exn
    Right () ->
      void . asyncDetached $ do
        heartbeatLostOrDone_ <- heartbeatLostOrDone
        doCleanup
        case heartbeatLostOrDone_ of
          Left (TimeoutException time) ->
            evilThrowTo mainThread $ HeartbeatLostException time
          _ -> return ()
  f results

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (RpcResponse res)
         , HasNamespace server
         , RpcTransport transport
         , Has transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt 'NoAuth server transport route req ('Direct res)) where
  makeClient t _ = callDirect @timeout @fmt @() @transport @server @route t

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (RpcResponse (StreamingResponse res))
         , HasNamespace server
         , RpcTransport transport
         , Has transport t
         , KnownSymbol route
         , Eq res
         ) =>
         Client t (Endpoint_ timeout fmt 'NoAuth server transport route req ('Streaming res)) where
  makeClient t _ = callStreaming @timeout @fmt @() @transport @server @route t

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (RpcResponse res)
         , HasNamespace server
         , Has (AuthSigner auth) t
         , RpcTransport transport
         , Has transport t
         , KnownSymbol route
         ) =>
         Client t (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Direct res)) where
  makeClient t _ = callDirect @timeout @fmt @auth @transport @server @route t

instance ( KnownNat timeout
         , Serializable fmt req
         , Deserializable fmt (RpcResponse (StreamingResponse res))
         , HasNamespace server
         , Has (AuthSigner auth) t
         , RpcTransport transport
         , Has transport t
         , KnownSymbol route
         , Eq res
         ) =>
         Client t (Endpoint_ timeout fmt ('Auth auth) server transport route req ('Streaming res)) where
  makeClient t _ = callStreaming @timeout @fmt @auth @transport @server @route t
