module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Routes spec where
  Routes (Endpoint _ (route :: Symbol) _ _) = '[ route]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (Endpoint _ (route :: Symbol) _ _) = '[ route]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data RpcServerException =
  AlreadyServing (Text' "Route")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (Endpoint 'NoAuth _ req ('Direct res)) = req -> IO res
  Handlers (Endpoint 'NoAuth _ req ('Streaming res)) = req -> IO (Streamly.Serial res)
  Handlers (Endpoint ('Auth auth) _ req ('Direct res)) = Claims auth -> req -> IO res
  Handlers (Endpoint ('Auth auth) _ req ('Streaming res)) = Claims auth -> req -> IO (Streamly.Serial res)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class (RpcTransport transport) =>
      Server spec transport
  where
  serve :: Handlers spec -> Proxy (spec, transport) -> transport -> IO ()

instance ( HasUniqueRoutes (a
                            :<|> b)
         , Server a transport
         , Server b transport
         ) =>
         Server (a
                 :<|> b) transport where
  serve ::
       (Handlers a
        :<|> Handlers b)
    -> Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO ()
  serve (aHandlers :<|> bHandlers) _ transport = do
    serve aHandlers (Proxy :: Proxy (a, transport)) transport
    serve bHandlers (Proxy :: Proxy (b, transport)) transport

directSender :: (res -> IO ()) -> res -> IO ()
directSender send = send

streamingSender :: (ResultItem res -> IO ()) -> Streamly.Serial res -> IO ()
streamingSender send resStream = do
  Streamly.mapM_ (send . Result) resStream
  send EndOfResults

_serve ::
     (Read req, FromJSON req, Show res, ToJSON res, RpcTransport transport)
  => ((res -> IO ()) -> x -> IO ())
     -- ^ x is typically res or Streamly.Serial res.
  -> (Headers -> Text' "Request" -> IO (Maybe (Claims auth)))
  -> (Claims auth -> req -> IO x)
  -> Text' "Route"
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

instance ( KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'NoAuth (route :: Symbol) req ('Direct res)) transport where
  serve ::
       (req -> IO res)
    -> Proxy (Endpoint 'NoAuth route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler _ =
    _serve
      directSender
      verifyEmpty
      (const handler)
      (proxyText' (Proxy :: Proxy route))

instance ( KnownSymbol route
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint 'NoAuth (route :: Symbol) req ('Streaming res)) transport where
  serve ::
       (req -> IO (Streamly.Serial res))
    -> Proxy (Endpoint 'NoAuth route req ('Streaming res), transport)
    -> transport
    -> IO ()
  serve handler _ =
    _serve
      streamingSender
      verifyEmpty
      (const handler)
      (proxyText' (Proxy :: Proxy route))

instance ( KnownSymbol route
         , AuthScheme auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint ('Auth auth) (route :: Symbol) req ('Direct res)) transport where
  serve ::
       (Claims auth -> req -> IO res)
    -> Proxy (Endpoint ('Auth auth) route req ('Direct res), transport)
    -> transport
    -> IO ()
  serve handler _ =
    _serve directSender verify handler (proxyText' (Proxy :: Proxy route))

instance ( KnownSymbol route
         , AuthScheme auth
         , Read req
         , Show res
         , FromJSON req
         , ToJSON res
         , RpcTransport transport
         ) =>
         Server (Endpoint ('Auth auth) (route :: Symbol) req ('Streaming res)) transport where
  serve ::
       (Claims auth -> req -> IO (Streamly.Serial res))
    -> Proxy (Endpoint ('Auth auth) route req ('Streaming res), transport)
    -> transport
    -> IO ()
  serve handler _ =
    _serve streamingSender verify handler (proxyText' (Proxy :: Proxy route))
