module Vest.Bridge.Rpc.Server
  ( module Vest.Bridge.Rpc.Server
  ) where

import qualified Stream
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
  Handlers (Endpoint_ _ _ ('Auth auth) t _ _ req ('Direct res)) = t -> AuthClaims auth -> req -> IO res
  Handlers (Endpoint_ _ _ ('Auth auth) t _ _ req ('Streaming res)) = t -> AuthClaims auth -> req -> IO (Stream res)
  Handlers (Endpoint_ _ _ 'NoAuth t _ _ req ('Direct res)) = t -> req -> IO res
  Handlers (Endpoint_ _ _ 'NoAuth t _ _ req ('Streaming res)) = t -> req -> IO (Stream res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

data RpcServerException =
  AlreadyServing (NamespacedText' "Route")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

class (HasNamespace t) =>
      Server t spec
  where
  serve :: Handlers spec -> t -> Proxy spec -> IO ()

instance HasNamespace t => Server t () where
  serve _ _ _ = return ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server t a
         , Server t b
         ) =>
         Server t (a
                   :<|> b) where
  serve ::
       (Handlers a
        :<|> Handlers b)
    -> t
    -> Proxy (a
              :<|> b)
    -> IO ()
  serve (aHandlers :<|> bHandlers) t _ = do
    serve aHandlers t (Proxy :: Proxy a)
    serve bHandlers t (Proxy :: Proxy b)

directSender :: (res -> IO ()) -> IO res -> IO ()
directSender send = (>>= send)

streamingSender ::
     Time Second -> (StreamingResponse res -> IO ()) -> IO (Stream res) -> IO ()
streamingSender _timeout send xs = do
  send Heartbeat
  -- (Tagged _postponeHeartbeat, Tagged cancelHeartbeats) <-
  --   intervalRenewable (timeoutsPerHeartbeat *:* timeout) (send Heartbeat)
  -- xs >>= Stream.mapM_ (\x -> postponeHeartbeat >> send (Result x))
  -- cancelHeartbeats
  xs >>= Stream.mapM_ (send . Result)
  send EndOfResults

serve_ ::
     forall fmt req res verifier transport x.
     ( Deserializable fmt req
     , Serializable fmt (Either RpcClientException res)
     , RequestVerifier verifier
     , RpcTransport transport
     )
  => ((res -> IO ()) -> IO x -> IO ())
     -- ^ x is typically res or Streamly.Serial res.
     -- res itself may be StreamingResponse a in the case of a streaming sender.
  -> verifier
  -> (VerifierClaims verifier -> req -> IO x)
  -> NamespacedText' "Route"
  -> transport
  -> IO ()
serve_ sender verifier handler = _consumeRequests asyncHandle
  where
    asyncHandle headers reqText respond =
      async $
      catch
        (do time <- now
            claims <-
              fromJustUnsafe BadAuth $
              verifyRequest verifier headers reqText time
            req <- catch (deserializeUnsafe' @fmt reqText) (throw . BadCall)
            sender
              (respond .
               serialize' @fmt @(Either RpcClientException res) . Right)
              (handler claims req))
        (respond . serialize' @fmt @(Either RpcClientException res) . Left)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , Deserializable fmt req
         , Serializable fmt (Either RpcClientException res)
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ _timeout fmt 'NoAuth t transport route req ('Direct res)) where
  serve ::
       (t -> req -> IO res)
    -> t
    -> Proxy (Endpoint_ _timeout fmt 'NoAuth t transport route req ('Direct res))
    -> IO ()
  serve handler t _ =
    serve_
      @fmt
      directSender
      ()
      (const $ handler t)
      (namespaced' @t $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , KnownNat timeout
         , Deserializable fmt req
         , Serializable fmt (Either RpcClientException (StreamingResponse res))
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ timeout fmt 'NoAuth t transport route req ('Streaming res)) where
  serve ::
       (t -> req -> IO (Stream res))
    -> t
    -> Proxy (Endpoint_ timeout fmt 'NoAuth t transport route req ('Streaming res))
    -> IO ()
  serve handler t _ =
    serve_
      @fmt
      (streamingSender $ natSeconds @timeout)
      ()
      (const $ handler t)
      (namespaced' @t $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasAuthVerifier auth t
         , HasRpcTransport transport t
         , Deserializable fmt req
         , Serializable fmt (Either RpcClientException res)
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ _timeout fmt ('Auth auth) t transport route req ('Direct res)) where
  serve ::
       (t -> AuthClaims auth -> req -> IO res)
    -> t
    -> Proxy (Endpoint_ _timeout fmt ('Auth auth) t transport route req ('Direct res))
    -> IO ()
  serve handler t _ =
    serve_
      @fmt
      directSender
      (authVerifier @auth t)
      (handler t)
      (namespaced' @t $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasAuthVerifier auth t
         , HasRpcTransport transport t
         , KnownNat timeout
         , Deserializable fmt req
         , Serializable fmt (Either RpcClientException (StreamingResponse res))
         , KnownSymbol route
         ) =>
         Server t (Endpoint_ timeout fmt ('Auth auth) t transport route req ('Streaming res)) where
  serve ::
       (t -> AuthClaims auth -> req -> IO (Stream res))
    -> t
    -> Proxy (Endpoint_ timeout fmt ('Auth auth) t transport route req ('Streaming res))
    -> IO ()
  serve handler t _ =
    serve_
      @fmt
      (streamingSender $ natSeconds @timeout)
      (authVerifier @auth t)
      (handler t)
      (namespaced' @t $ symbolText' (Proxy :: Proxy route))
      (rpcTransport @transport t)
