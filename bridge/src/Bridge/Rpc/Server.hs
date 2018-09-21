module Bridge.Rpc.Server
  ( module Bridge.Rpc.Server
  ) where

import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Routes spec where
  Routes (DirectEndpointAs (f :: Format) (s :: Symbol) a b) = '[ s]
  Routes (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) = '[ s]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (DirectEndpointAs (f :: Format) (s :: Symbol) a b) = '[ s]
  NubRoutes (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) = '[ s]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data RpcServerException =
  AlreadyServing (Id "Rpc")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec where
  Handlers (DirectEndpointAs (f :: Format) (s :: Symbol) a b) = a -> IO b
  Handlers (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) = a -> IO (Streamly.Serial b)
  Handlers (a
            :<|> b) = (Handlers a
                       :<|> Handlers b)

class (RpcTransport transport) =>
      Server spec transport
  where
  serve :: Proxy (spec, transport) -> transport -> Handlers spec -> IO ()

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
    -> transport
    -> (Handlers a
        :<|> Handlers b)
    -> IO ()
  serve _ transport (aHandlers :<|> bHandlers) = do
    serve (Proxy :: Proxy (a, transport)) transport aHandlers
    serve (Proxy :: Proxy (b, transport)) transport bHandlers

instance (KnownSymbol s, Read a, Show b, RpcTransport transport) =>
         Server (DirectEndpoint (s :: Symbol) a b) transport where
  serve ::
       Proxy (DirectEndpoint s a b, transport)
    -> transport
    -> (a -> IO b)
    -> IO ()
  serve _ = _serve (. show) read (Id $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, Read a, Show b, RpcTransport transport) =>
         Server (StreamingEndpoint (s :: Symbol) a b) transport where
  serve ::
       Proxy (StreamingEndpoint s a b, transport)
    -> transport
    -> (a -> IO (Streamly.Serial b))
    -> IO ()
  serve _ =
    _serve
      (\pub xs -> do
         Streamly.mapM_ (pub . show . Result) xs
         pub $ show $ EndOfResults @b)
      read
      (Id $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, FromJSON a, ToJSON b, RpcTransport transport) =>
         Server (DirectEndpointJSON (s :: Symbol) a b) transport where
  serve ::
       Proxy (DirectEndpointJSON s a b, transport)
    -> transport
    -> (a -> IO b)
    -> IO ()
  serve _ = _serve (. encode) decode (Id $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, FromJSON a, ToJSON b, RpcTransport transport) =>
         Server (StreamingEndpointJSON (s :: Symbol) a b) transport where
  serve ::
       Proxy (StreamingEndpointJSON s a b, transport)
    -> transport
    -> (a -> IO (Streamly.Serial b))
    -> IO ()
  serve _ =
    _serve
      (\pub xs -> do
         Streamly.mapM_ (pub . encode . Result) xs
         pub . encode $ EndOfResults @b)
      decode
      (Id $ proxyText (Proxy :: Proxy s))
