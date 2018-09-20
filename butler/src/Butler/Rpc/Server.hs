module Butler.Rpc.Server
  ( module Butler.Rpc.Server
  ) where

import Butler.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

data RpcServerException =
  AlreadyServing Route
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Handlers spec :: *

type instance
     Handlers (DirectEndpointAs (f :: Format) (s :: Symbol) a b) =
     a -> IO b

type instance
     Handlers (StreamingEndpointAs (f :: Format) (s :: Symbol) a b) =
     a -> IO (Streamly.Serial b)

type instance Handlers (a :<|> b) = Handlers a :<|> Handlers b

class (RpcTransport transport) =>
      Server spec transport
  where
  serve :: Proxy (spec, transport) -> transport -> Handlers spec -> IO ()

instance (Server a transport, Server b transport) =>
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
  serve _ = _serve (. show) read (Route $ proxyText (Proxy :: Proxy s))

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
      (Route $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, FromJSON a, ToJSON b, RpcTransport transport) =>
         Server (DirectEndpointJSON (s :: Symbol) a b) transport where
  serve ::
       Proxy (DirectEndpointJSON s a b, transport)
    -> transport
    -> (a -> IO b)
    -> IO ()
  serve _ = _serve (. encode) decode (Route . proxyText $ (Proxy :: Proxy s))

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
      (Route $ proxyText (Proxy :: Proxy s))
