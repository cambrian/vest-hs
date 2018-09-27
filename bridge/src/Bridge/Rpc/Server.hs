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

serveProcessor ::
     (Read req, FromJSON req, Show res, ToJSON res)
  => (Headers -> Text' "Request" -> IO (Maybe (Claims auth)))
  -> ((res -> IO ()) -> x -> IO ()) -- Type x is typically res or Streamly.Serial res.
  -> (Claims auth -> req -> IO x) -- For non authenticated endpoints, Claims auth will be ().
  -> (Text' "Response" -> IO ())
  -> Headers
  -> Text' "Request"
  -> IO ()
serveProcessor auth sender handler send headers reqText = do
  let Headers {format} = headers
      (serialize', deserializeUnsafe') = runtimeSerializationsOf' format
  catch
    (do claims <- auth headers reqText >>= fromJustUnsafe BadAuth
        req <- deserializeUnsafe' reqText -- TODO: catch and rethrow
        res <- handler claims req
        sender (send . serialize' . Right) res)
    (\(e :: RpcClientException) -> send . serialize' . Left $ e)

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
  serve _ handler =
    _serve
      (serveProcessor verifyEmpty streamingSender (const handler))
      (proxyText' (Proxy :: Proxy route))

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
  serve _ handler =
    _serve
      (serveProcessor verifyEmpty directSender (const handler))
      (proxyText' (Proxy :: Proxy route))

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
  serve _ handler =
    _serve
      (serveProcessor verify streamingSender handler)
      (proxyText' (Proxy :: Proxy route))

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
  serve _ handler =
    _serve
      (serveProcessor verify directSender handler)
      (proxyText' (Proxy :: Proxy route))
