module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest.Prelude

type family Routes spec where
  Routes () = '[]
  Routes (Endpoint _ _ _ (route :: Symbol) _ _) = '[ route]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes () = '[]
  NubRoutes (Endpoint _ _ _ (route :: Symbol) _ _) = '[ route]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

type family Handlers spec where
  Handlers () = ()
  Handlers (Endpoint t 'NoAuth _ _ req ('Direct res)) = t -> req -> IO res
  Handlers (Endpoint t 'NoAuth _ _ req ('Streaming res)) = t -> req -> IO (Streamly.Serial res)
  Handlers (Endpoint t ('Auth auth) _ _ req ('Direct res)) = t -> Claims auth -> req -> IO res
  Handlers (Endpoint t ('Auth auth) _ _ req ('Streaming res)) = t -> Claims auth -> req -> IO (Streamly.Serial res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

data RpcServerException =
  AlreadyServing (NamespacedText' "Route")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

class (Auth a) =>
      HasAuth a t
  where
  auth :: t -> a

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

directSender :: (res -> IO ()) -> res -> IO ()
directSender send = send

streamingSender :: (ResultItem res -> IO ()) -> Streamly.Serial res -> IO ()
streamingSender send resStream = do
  Streamly.mapM_ (send . Result) resStream
  send EndOfResults

verifyEmpty :: Headers -> Text' "Request" -> Maybe ()
verifyEmpty _ _ = Just ()

_serve ::
     (Read req, FromJSON req, Show res, ToJSON res, RpcTransport transport)
  => ((res -> IO ()) -> x -> IO ())
     -- ^ x is typically res or Streamly.Serial res.
  -> (Headers -> Text' "Request" -> Maybe claims)
  -> (claims -> req -> IO x)
  -> NamespacedText' "Route"
  -> transport
  -> IO ()
_serve sender verifyAuth handler = _consumeRequests asyncHandle
  where
    asyncHandle headers reqText respond =
      async $ do
        let fmt = format headers
            (serialize_, deserializeUnsafe_) = runtimeSerializationsOf' fmt
        catch
          (do claims <- fromJustUnsafe BadAuth $ verifyAuth headers reqText
              req <-
                catchAny
                  (deserializeUnsafe_ reqText)
                  (const $ throw $ BadCall (fmt, reqText))
              res <- handler claims req
              sender (respond . serialize_ . Right) res)
          (\(e :: RpcClientException) -> respond . serialize_ . Left $ e)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         ) =>
         Server t (Endpoint t 'NoAuth transport route req ('Direct res)) where
  serve ::
       (t -> req -> IO res)
    -> t
    -> Proxy (Endpoint t 'NoAuth transport route req ('Direct res))
    -> IO ()
  serve handler t _ =
    _serve
      directSender
      verifyEmpty
      (const $ handler t)
      (namespaced' @t $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasRpcTransport transport t
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         ) =>
         Server t (Endpoint t 'NoAuth transport route req ('Streaming res)) where
  serve ::
       (t -> req -> IO (Streamly.Serial res))
    -> t
    -> Proxy (Endpoint t 'NoAuth transport route req ('Streaming res))
    -> IO ()
  serve handler t _ =
    _serve
      streamingSender
      verifyEmpty
      (const $ handler t)
      (namespaced' @t $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasAuth auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         ) =>
         Server t (Endpoint t ('Auth auth) transport route req ('Direct res)) where
  serve ::
       (t -> Claims auth -> req -> IO res)
    -> t
    -> Proxy (Endpoint t ('Auth auth) transport route req ('Direct res))
    -> IO ()
  serve handler t _ =
    _serve
      directSender
      (verify (auth t))
      (handler t)
      (namespaced' @t $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)

instance ( HasNamespace t
         , HasAuth auth t
         , HasRpcTransport transport t
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         ) =>
         Server t (Endpoint t ('Auth auth) transport route req ('Streaming res)) where
  serve ::
       (t -> Claims auth -> req -> IO (Streamly.Serial res))
    -> t
    -> Proxy (Endpoint t ('Auth auth) transport route req ('Streaming res))
    -> IO ()
  serve handler t _ =
    _serve
      streamingSender
      (verify (auth t))
      (handler t)
      (namespaced' @t $ proxyText' (Proxy :: Proxy route))
      (rpcTransport @transport t)
