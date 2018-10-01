module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Routes spec where
  Routes (Endpoint _ _ (route :: Symbol) _ _) = '[ route]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (Endpoint _ _ (route :: Symbol) _ _) = '[ route]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data RpcServerException =
  AlreadyServing (Text' "Route")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (Endpoint 'Direct 'NoAuth _ req res) = req -> IO res
  Handlers (Endpoint 'Streaming 'NoAuth _ req res) = req -> IO (Streamly.Serial res)
  Handlers (Endpoint 'Direct ('Auth auth) _ req res) = Claims auth -> req -> IO res
  Handlers (Endpoint 'Streaming ('Auth auth) _ req res) = Claims auth -> req -> IO (Streamly.Serial res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class (RpcTransport transport) =>
      Server spec transport
  where
  serve :: Proxy (spec, transport) -> Handlers spec -> transport -> IO ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server a transport
         , Server b transport
         ) =>
         Server (a
                 :<|> b) transport where
  serve ::
       Proxy ( a
               :<|> b
             , transport)
    -> (Handlers a
        :<|> Handlers b)
    -> transport
    -> IO ()
  serve _ (aHandlers :<|> bHandlers) transport = do
    serve (Proxy :: Proxy (a, transport)) aHandlers transport
    serve (Proxy :: Proxy (b, transport)) bHandlers transport

directSender :: (res -> IO ()) -> res -> IO ()
directSender send = send

streamingSender :: (ResultItem res -> IO ()) -> Streamly.Serial res -> IO ()
streamingSender send resStream = do
  Streamly.mapM_ (send . Result) resStream
  send EndOfResults

_serve ::
     (Read req, FromJSON req, Show res, ToJSON res, RpcTransport transport)
  => ((res -> IO ()) -> x -> IO ()) -- Type x is typically res or Streamly.Serial res.
  -> (Headers -> Text' "Request" -> IO (Maybe (Claims auth)))
  -> Text' "Route"
  -> (Claims auth -> req -> IO x)
  -> transport
  -> IO ()
_serve sender verifyAuth route handler transport =
  _consumeRequests asyncHandle route transport
  where
    asyncHandle headers reqText respond = async $ do
      let fmt = format headers
          (serialize_, deserializeUnsafe_) = runtimeSerializationsOf' $ fmt
      catch
        (do claims <- verifyAuth headers reqText >>= fromJustUnsafe BadAuth
            req <-
              catchAny
                (deserializeUnsafe_ reqText)
                (const $ throw $ BadCall (fmt, reqText))
            res <- handler claims req
            sender (respond . serialize_ . Right) res)
        (\(e :: RpcClientException) -> respond . serialize_ . Left $ e)

instance ( KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct 'NoAuth (route :: Symbol) req res) transport where
  serve ::
       Proxy (Endpoint 'Direct 'NoAuth route req res, transport)
    -> (req -> IO res)
    -> transport
    -> IO ()
  serve _ handler = _serve directSender verifyEmpty (proxyText' (Proxy :: Proxy route)) (const handler)

instance ( KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming 'NoAuth (route :: Symbol) req res) transport where
  serve ::
       Proxy (Endpoint 'Streaming 'NoAuth route req res, transport)
    -> (req -> IO (Streamly.Serial res))
    -> transport
    -> IO ()
  serve _ handler = _serve streamingSender verifyEmpty (proxyText' (Proxy :: Proxy route)) (const handler)

instance ( KnownSymbol route
         , AuthScheme auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct ('Auth auth) (route :: Symbol) req res) transport where
  serve ::
       Proxy (Endpoint 'Direct ('Auth auth) route req res, transport)
    -> (Claims auth -> req -> IO res)
    -> transport
    -> IO ()
  serve _ = _serve directSender verify (proxyText' (Proxy :: Proxy route))

instance ( KnownSymbol route
         , AuthScheme auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming ('Auth auth) (route :: Symbol) req res) transport where
  serve ::
       Proxy (Endpoint 'Streaming ('Auth auth) route req res, transport)
    -> (Claims auth -> req -> IO (Streamly.Serial res))
    -> transport
    -> IO ()
  serve _ = _serve streamingSender verify (proxyText' (Proxy :: Proxy route))
