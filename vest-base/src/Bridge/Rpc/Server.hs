module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest.Prelude

data RpcServerException =
  AlreadyServing (NamespacedText' "Route")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (Endpoint t 'NoAuth _ req ('Direct res)) = t -> req -> IO res
  Handlers (Endpoint t 'NoAuth _ req ('Streaming res)) = t -> req -> IO (Streamly.Serial res)
  Handlers (Endpoint t ('Auth auth) _ req ('Direct res)) = t -> Claims auth -> req -> IO res
  Handlers (Endpoint t ('Auth auth) _ req ('Streaming res)) = t -> Claims auth -> req -> IO (Streamly.Serial res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class (Auth a) =>
      HasAuth a t
  where
  auth :: t -> a

class (HasNamespace t, RpcTransport transport) =>
      Server t spec transport
  where
  serve :: Handlers spec -> t -> Proxy (spec, transport) -> transport -> IO ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server t a transport
         , Server t b transport
         ) =>
         Server t (a
                   :<|> b) transport where
  serve ::
       (Handlers a
        :<|> Handlers b)
    -> t
    -> Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO ()
  serve (aHandlers :<|> bHandlers) t _ transport = do
    serve aHandlers t (Proxy :: Proxy (a, transport)) transport
    serve bHandlers t (Proxy :: Proxy (b, transport)) transport

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
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server t (Endpoint t 'NoAuth route req ('Direct res)) transport where
  serve ::
       (t -> req -> IO res)
    -> t
    -> Proxy (Endpoint t 'NoAuth route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler t _ =
    _serve
      directSender
      verifyEmpty
      (const $ handler t)
      (namespaced @t $ proxyText' (Proxy :: Proxy route))

instance ( HasNamespace t
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server t (Endpoint t 'NoAuth route req ('Streaming res)) transport where
  serve ::
       (t -> req -> IO (Streamly.Serial res))
    -> t
    -> Proxy (Endpoint t 'NoAuth route req ('Streaming res), transport)
    -> transport
    -> IO ()
  serve handler t _ =
    _serve
      streamingSender
      verifyEmpty
      (const $ handler t)
      (namespaced @t $ proxyText' (Proxy :: Proxy route))

instance ( HasNamespace t
         , HasAuth auth t
         , KnownSymbol route
         , Auth auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server t (Endpoint t ('Auth auth) route req ('Direct res)) transport where
  serve ::
       (t -> Claims auth -> req -> IO res)
    -> t
    -> Proxy (Endpoint t ('Auth auth) route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler t _ =
    _serve
      directSender
      (verify (auth t))
      (handler t)
      (namespaced @t $ proxyText' (Proxy :: Proxy route))

instance ( HasNamespace t
         , HasAuth auth t
         , KnownSymbol route
         , Auth auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server t (Endpoint t ('Auth auth) route req ('Streaming res)) transport where
  serve ::
       (t -> Claims auth -> req -> IO (Streamly.Serial res))
    -> t
    -> Proxy (Endpoint t ('Auth auth) route req ('Streaming res), transport)
    -> transport
    -> IO ()
  serve handler t _ =
    _serve
      streamingSender
      (verify (auth t))
      (handler t)
      (namespaced @t $ proxyText' (Proxy :: Proxy route))
