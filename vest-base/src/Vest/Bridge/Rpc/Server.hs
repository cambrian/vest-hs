module Vest.Bridge.Rpc.Server
  ( Server
  , Handlers
  , AlreadyServingException(..)
  , serve
  ) where

import Vest.Bridge.Rpc.Auth
import Vest.Bridge.Rpc.Prelude
import Vest.Prelude

type family Routes spec where
  Routes () = '[]
  Routes (Endpoint_ _ _ _ _ _ (route :: Symbol) _ _) = '[ route]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes () = '[]
  NubRoutes (Endpoint_ _ _ _ _ _ (route :: Symbol) _ _) = '[ route]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

type family Handlers spec where
  Handlers () = ()
  Handlers (Endpoint_ _ _ ('Auth auth) _ _ _ req ('Direct res)) = AuthClaims auth -> req -> IO res
  Handlers (Endpoint_ _ _ ('Auth auth) _ _ _ req ('Streaming res)) = AuthClaims auth -> req -> IO (Stream ValueBuffer res)
  Handlers (Endpoint_ _ _ 'NoAuth _ _ _ req ('Direct res)) = req -> IO res
  Handlers (Endpoint_ _ _ 'NoAuth _ _ _ req ('Streaming res)) = req -> IO (Stream ValueBuffer res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class Server t spec where
  serve :: t -> Proxy spec -> Handlers spec -> IO ()

instance Server t () where
  serve _ _ _ = return ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server t a
         , Server t b
         ) =>
         Server t (a
                   :<|> b) where
  serve t _ (aHandlers :<|> bHandlers) = do
    serve t (Proxy :: Proxy a) aHandlers
    serve t (Proxy :: Proxy b) bHandlers

directSender :: (res -> IO ()) -> IO res -> IO ()
directSender send = (>>= send)

streamingSender ::
     Eq res
  => Duration
  -> (StreamingResponse res -> IO ())
  -> IO (Stream ValueBuffer res)
  -> IO ()
streamingSender timeout send xs = do
  send Heartbeat
  (Tagged postponeHeartbeat, Tagged cancelHeartbeats) <-
    intervalRenewable (timeoutsPerHeartbeat *^ timeout) (send Heartbeat)
  xs >>= consumeStream (\x -> postponeHeartbeat >> send (Result x))
  cancelHeartbeats
  send EndOfResults

serve_ ::
     forall fmt auth transport route t req res x.
     ( Deserializable fmt req
     , Serializable fmt (RpcResponse res)
     , HasAuthVerifier auth t
     , HasRpcTransport transport t
     , HasNamespace t
     , KnownSymbol route
     )
  => ((res -> IO ()) -> IO x -> IO ())
     -- ^ x is typically res or Streamly.Serial res.
     -- res itself may be StreamingResponse a in the case of a streaming sender.
  -> t
  -> (AuthClaims auth -> req -> IO x)
  -> IO ()
serve_ sender t handler =
  serveRaw (rpcTransport @transport t) rawRoute asyncHandle
  where
    rawRoute = serialize' @'Pretty $ namespaced @t (Proxy :: Proxy route)
    asyncHandle headers reqText respond =
      asyncThrows $
      catches
        (do claims <-
              atomically (verifyRequest (authVerifier @auth t) headers reqText) >>=
              fromRightOrThrowLeft
            req <- deserializeUnsafe' @fmt reqText
            sender (sendToClient . RpcResponse) (handler claims req))
        [ Handler $ \(x :: AuthException) -> do
            sendToClient $ RpcResponseClientException $ show x
            log Warn "Handler exception" x
        , Handler $ \(x :: InvalidCallException) -> do
            sendToClient $ RpcResponseClientException $ show x
            log Warn "Handler exception" x
        , Handler $ \(x :: DeserializeException fmt) -> do
            sendToClient $ RpcResponseClientException $ show x
            log Warn "Handler exception" x
        , Handler $ \(x :: SomeException) -> do
            sendToClient RpcResponseServerException
            log Error "Handler server exception" x
            -- ^ TODO: Send a generic 503 type message instead of `show x` and add logging.
            -- throw x
        ]
      where
        sendToClient = respond . serialize' @fmt @(RpcResponse res)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , Deserializable fmt req
         , Serializable fmt (RpcResponse res)
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ _timeout fmt 'NoAuth t transport route req ('Direct res)) where
  serve t _ handler =
    serve_ @fmt @() @transport @route directSender t (const handler)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , KnownNat timeout
         , Deserializable fmt req
         , Serializable fmt (RpcResponse (StreamingResponse res))
         , KnownSymbol route
         , Eq res
         ) =>
         Server t (Endpoint_ timeout fmt 'NoAuth t transport route req ('Streaming res)) where
  serve t _ handler =
    serve_
      @fmt
      @()
      @transport
      @route
      (streamingSender $ natSeconds @timeout)
      t
      (const handler)

instance ( HasNamespace t
         , HasAuthVerifier auth t
         , HasRpcTransport transport t
         , Deserializable fmt req
         , Serializable fmt (RpcResponse res)
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ _timeout fmt ('Auth auth) t transport route req ('Direct res)) where
  serve t _ = serve_ @fmt @auth @transport @route directSender t

instance ( HasNamespace t
         , HasAuthVerifier auth t
         , HasRpcTransport transport t
         , KnownNat timeout
         , Deserializable fmt req
         , Serializable fmt (RpcResponse (StreamingResponse res))
         , KnownSymbol route
         , Eq res
         ) =>
         Server t (Endpoint_ timeout fmt ('Auth auth) t transport route req ('Streaming res)) where
  serve t _ =
    serve_
      @fmt
      @auth
      @transport
      @route
      (streamingSender $ natSeconds @timeout)
      t
