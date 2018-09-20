module Butler.PubSub.Publisher
  ( module Butler.PubSub.Publisher
  ) where

import Butler.PubSub.Prelude
import qualified Streamly
import VestPrelude

type family Streams spec :: *

type instance Streams (TopicAs (f :: Format) (s :: Symbol) a) =
     Streamly.Serial a

type instance Streams (a :<|> b) = Streams a :<|> Streams b

class (PubSubTransport transport) =>
      Publisher spec transport
  where
  publish :: Proxy (spec, transport) -> transport -> Streams spec -> IO ()

instance (Publisher a transport, Publisher b transport) =>
         Publisher (a
                    :<|> b) transport where
  publish ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> (Streams a
        :<|> Streams b)
    -> IO ()
  publish _ transport (aStreams :<|> bStreams) = do
    publish (Proxy :: Proxy (a, transport)) transport aStreams
    publish (Proxy :: Proxy (b, transport)) transport bStreams

instance (KnownSymbol route, Show a, PubSubTransport transport) =>
         Publisher (Topic (route :: Symbol) a) transport where
  publish ::
       Proxy (Topic route a, transport)
    -> transport
    -> Streamly.Serial a
    -> IO ()
  publish _ = _publish show (Route $ proxyText $ (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON a, PubSubTransport transport) =>
         Publisher (TopicJSON (route :: Symbol) a) transport where
  publish ::
       Proxy (TopicJSON route a, transport)
    -> transport
    -> Streamly.Serial a
    -> IO ()
  publish _ = _publish encode (Route $ proxyText $ (Proxy :: Proxy route))
