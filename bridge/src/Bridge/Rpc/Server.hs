module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Auth
import Bridge.Prelude
import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Routes spec where
  Routes (Endpoint (r :: Arity) (h :: Auth) (s :: Symbol) a b) = '[ s]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (Endpoint (r :: Arity) (h :: Auth) (s :: Symbol) a b) = '[ s]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data RpcServerException =
  AlreadyServing (Id "Rpc")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (Endpoint 'Direct 'OpenAuth (s :: Symbol) a b) = a -> IO b
  Handlers (Endpoint 'Direct _ (s :: Symbol) a b) = Claims -> a -> IO b
  Handlers (Endpoint 'Streaming 'OpenAuth (s :: Symbol) a b) = a -> IO (Streamly.Serial b)
  Handlers (Endpoint 'Streaming _ (s :: Symbol) a b) = Claims -> a -> IO (Streamly.Serial b)
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

authenticatorOf ::
     Auth -> (Headers -> Text -> (Claims -> IO ()) -> IO () -> IO ())
authenticatorOf =
  \case
    OpenAuth -> verifyOpen
    TokenAuth -> verifyToken

directSender :: (ResultItem res -> IO ()) -> res -> IO ()
directSender send = send . Result

streamingSender :: (ResultItem res -> IO ()) -> Streamly.Serial res -> IO ()
streamingSender send resStream = do
  Streamly.mapM_ (send . Result) resStream
  send EndOfResults

serveProcessor ::
     (Read req, FromJSON req, Show res, ToJSON res)
  => (Headers -> Text -> (Claims -> IO ()) -> IO () -> IO ())
  -> (Claims -> req -> IO x) -- For OpenAuth endpoints, pass a wrapped handler that ignores claims.
  -> ((ResultItem res -> IO ()) -> x -> IO ()) -- Type x contains one or many of type r.
  -> (Text -> IO ())
  -> Headers
  -> Text
  -> IO ()
serveProcessor authenticator handler sender send headers reqText = do
  let Headers {format} = headers
  let deserialize = deserializeOf format
      serialize = serializeOf format
  catch
    (authenticator
       headers
       reqText
       (\claims ->
          case deserialize reqText of
            Nothing -> throw $ BadCall reqText
            Just req -> do
              res <- handler claims req
              -- ResultItem is Either Exception Text.
              sender (send . serialize . Right) res)
       (throw BadAuth))
    (\e -> do
       let exc = (e :: RpcClientException)
       send . serialize . Left $ exc)

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct 'OpenAuth (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Direct 'OpenAuth s a b, transport)
    -> (a -> IO b)
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor (authenticatorOf OpenAuth) (\_ -> handler) directSender)
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming 'OpenAuth (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Streaming 'OpenAuth s a b, transport)
    -> (a -> IO (Streamly.Serial b))
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor (authenticatorOf OpenAuth) (\_ -> handler) streamingSender)
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Direct 'TokenAuth (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Direct 'TokenAuth s a b, transport)
    -> (Claims -> a -> IO b)
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor (authenticatorOf TokenAuth) handler directSender)
      (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Read a
         , Show b
         , FromJSON a
         , ToJSON b
         , RpcTransport transport
         ) =>
         Server (Endpoint 'Streaming 'TokenAuth (s :: Symbol) a b) transport where
  serve ::
       Proxy (Endpoint 'Streaming 'TokenAuth s a b, transport)
    -> (Claims -> a -> IO (Streamly.Serial b))
    -> transport
    -> IO ()
  serve _ handler =
    _serve
      (serveProcessor (authenticatorOf TokenAuth) handler streamingSender)
      (Id $ proxyText (Proxy :: Proxy s))
