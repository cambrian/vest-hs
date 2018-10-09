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
  Handlers (Endpoint service 'NoAuth _ req ('Direct res)) = service -> req -> IO res
  Handlers (Endpoint service 'NoAuth _ req ('Streaming res)) = service -> req -> IO (Streamly.Serial res)
  Handlers (Endpoint service ('Auth auth) _ req ('Direct res)) = service -> Claims auth -> req -> IO res
  Handlers (Endpoint service ('Auth auth) _ req ('Streaming res)) = service -> Claims auth -> req -> IO (Streamly.Serial res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class (Service service, RpcTransport transport) =>
      Server service spec transport
  where
  serve ::
       Handlers spec -> service -> Proxy (spec, transport) -> transport -> IO ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server service a transport
         , Server service b transport
         ) =>
         Server service (a
                         :<|> b) transport where
  serve ::
       (Handlers a
        :<|> Handlers b)
    -> service
    -> Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO ()
  serve (aHandlers :<|> bHandlers) service _ transport = do
    serve aHandlers service (Proxy :: Proxy (a, transport)) transport
    serve bHandlers service (Proxy :: Proxy (b, transport)) transport

directSender :: (res -> IO ()) -> res -> IO ()
directSender send = send

streamingSender :: (ResultItem res -> IO ()) -> Streamly.Serial res -> IO ()
streamingSender send resStream = do
  Streamly.mapM_ (send . Result) resStream
  send EndOfResults

verifyEmpty :: Headers -> Text' "Request" -> IO (Maybe ())
verifyEmpty _ _ = return $ Just ()

_serve ::
     (Read req, FromJSON req, Show res, ToJSON res, RpcTransport transport)
  => ((res -> IO ()) -> x -> IO ())
     -- ^ x is typically res or Streamly.Serial res.
  -> (Headers -> Text' "Request" -> IO (Maybe claims))
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
          (do claims <- verifyAuth headers reqText >>= fromJustUnsafe BadAuth
              req <-
                catchAny
                  (deserializeUnsafe_ reqText)
                  (const $ throw $ BadCall (fmt, reqText))
              res <- handler claims req
              sender (respond . serialize_ . Right) res)
          (\(e :: RpcClientException) -> respond . serialize_ . Left $ e)

instance ( Service service
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server service (Endpoint service 'NoAuth route req ('Direct res)) transport where
  serve ::
       (service -> req -> IO res)
    -> service
    -> Proxy (Endpoint service 'NoAuth route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler service _ =
    _serve
      directSender
      verifyEmpty
      (const $ handler service)
      (namespace @service $ proxyText' (Proxy :: Proxy route))

instance ( Service service
         , KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server service (Endpoint service 'NoAuth route req ('Streaming res)) transport where
  serve ::
       (service -> req -> IO (Streamly.Serial res))
    -> service
    -> Proxy (Endpoint service 'NoAuth route req ('Streaming res), transport)
    -> transport
    -> IO ()
  serve handler service _ =
    _serve
      streamingSender
      verifyEmpty
      (const $ handler service)
      (namespace @service $ proxyText' (Proxy :: Proxy route))

instance ( Service service
         , KnownSymbol route
         , Auth auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server service (Endpoint service ('Auth auth) route req ('Direct res)) transport where
  serve ::
       (service -> Claims auth -> req -> IO res)
    -> service
    -> Proxy (Endpoint service ('Auth auth) route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler service _ =
    _serve
      directSender
      verify
      (handler service)
      (namespace @service $ proxyText' (Proxy :: Proxy route))

instance ( Service service
         , KnownSymbol route
         , Auth auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server service (Endpoint service ('Auth auth) route req ('Streaming res)) transport where
  serve ::
       (service -> Claims auth -> req -> IO (Streamly.Serial res))
    -> service
    -> Proxy ( Endpoint service ('Auth auth) route req ('Streaming res)
             , transport)
    -> transport
    -> IO ()
  serve handler service _ =
    _serve
      streamingSender
      verify
      (handler service)
      (namespace @service $ proxyText' (Proxy :: Proxy route))
