module Bridge.PubSub.Subscriber
  ( module Bridge.PubSub.Subscriber
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import VestPrelude

-- The bound streams close iff unsubscribe is called
type family SubscriberBindings spec :: *

type instance
     SubscriberBindings (TopicAs (f :: Format) (s :: Symbol) a) =
     (Id, Streamly.Serial a)

type instance SubscriberBindings (a :<|> b) =
     SubscriberBindings a :<|> SubscriberBindings b

class (PubSubTransport transport) =>
      Subscriber spec transport
  where
  makeSubscriber ::
       Proxy (spec, transport) -> transport -> IO (SubscriberBindings spec)

instance (Subscriber a transport, Subscriber b transport) =>
         Subscriber (a
                     :<|> b) transport where
  makeSubscriber ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO (SubscriberBindings a
           :<|> SubscriberBindings b)
  makeSubscriber _ transport = do
    aStreams <- makeSubscriber (Proxy :: Proxy (a, transport)) transport
    bStreams <- makeSubscriber (Proxy :: Proxy (b, transport)) transport
    return $ aStreams :<|> bStreams

instance (KnownSymbol route, Read a, PubSubTransport transport) =>
         Subscriber (Topic (route :: Symbol) a) transport where
  makeSubscriber ::
       Proxy (Topic route a, transport)
    -> transport
    -> IO (Id, Streamly.Serial a)
  makeSubscriber _ =
    _subscribe readUnsafe (Route $ proxyText $ (Proxy :: Proxy route))

instance (KnownSymbol route, FromJSON a, PubSubTransport transport) =>
         Subscriber (TopicJSON (route :: Symbol) a) transport where
  makeSubscriber ::
       Proxy (TopicJSON route a, transport)
    -> transport
    -> IO (Id, Streamly.Serial a)
  makeSubscriber _ =
    _subscribe decodeUnsafe (Route $ proxyText $ (Proxy :: Proxy route))
