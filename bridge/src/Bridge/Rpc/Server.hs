module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Prelude
import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Routes spec where
  Routes (Endpoint (r :: Arity) (auth :: Maybe *) (s :: Symbol) a b) = '[ s]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (Endpoint (r :: Arity) (auth :: Maybe *) (s :: Symbol) a b) = '[ s]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data RpcServerException =
  AlreadyServing (Id "Rpc")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (Endpoint 'Direct 'Nothing (s :: Symbol) a b) = a -> IO b
  Handlers (Endpoint 'Streaming 'Nothing (s :: Symbol) a b) = a -> IO (Streamly.Serial b)
  Handlers (Endpoint 'Direct ('Just auth) (s :: Symbol) a b) = Claims auth -> a -> IO b
  Handlers (Endpoint 'Streaming ('Just auth) (s :: Symbol) a b) = Claims auth -> a -> IO (Streamly.Serial b)
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
  => (Headers -> Id "RequestText" -> IO (Maybe (Claims auth)))
  -> ((res -> IO ()) -> x -> IO ()) -- Type x is typically res or Streamly.Serial res.
  -> (Claims auth -> req -> IO x) -- For non authenticated endpoints, Claims auth will be ().
  -> (Id "ResponseText" -> IO ())
  -> Headers
  -> Id "RequestText"
  -> IO ()
serveProcessor auth sender handler send headers reqText = do
  let Headers {format} = headers
      deserialize (Id x) = deserializeOf format $ x
      serialize = serializeOf format
  catch
    (do claims <- auth headers reqText >>= fromJustUnsafe BadAuth
        req <- fromJustUnsafe (BadCall reqText) $ deserialize reqText
        res <- handler claims req
        sender (send . Id @"ResponseText" . serialize . Right) res)
    (\(e :: RpcClientException) ->
       send . Id @"ResponseText" . serialize . Left $ e)

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming 'Nothing (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Streaming 'Nothing s a b, transport)
    -> (a -> IO (Streamly.Serial b))
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor verifyEmpty streamingSender (const handler))
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct 'Nothing (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Direct 'Nothing s a b, transport)
    -> (a -> IO b)
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor verifyEmpty directSender (const handler))
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Auth auth
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming ('Just auth) (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Streaming ('Just auth) s a b, transport)
    -> (Claims auth -> a -> IO (Streamly.Serial b))
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor verify streamingSender handler)
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Auth auth
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct ('Just auth) (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Direct ('Just auth) s a b, transport)
    -> (Claims auth -> a -> IO b)
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor verify directSender handler)
      (Id $ proxyText (Proxy :: Proxy s))
