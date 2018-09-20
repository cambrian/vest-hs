module Butler.PubSub
  ( module Butler.PubSub
  ) where

import Butler.Dsl
import qualified Streamly
import VestPrelude

class PubSubTransport t where
  _publish :: (a -> Text) -> Route -> t -> Streamly.Serial a -> IO ()
  -- _subscribe should mutate t to store the details necessary for unsubscribe
  -- SubscribeOne (non-streaming version) is deliberately unimplemented, because RabbitMQ does not
  -- support message history. Candidate solutions are building a separate pub/sub system on Kafka
  -- (or similar), or adding Cassandra to RabbitMQ. For now, you can use makeStreamVar in
  -- conjunction with subscribe' to get one-off values from a published topic.
  -- Returns subscriber ID and result stream as tuple (ID, stream).
  _subscribe :: (Text -> IO a) -> Route -> t -> IO (Id, Streamly.Serial a)
  unsubscribe :: t -> Id -> IO ()

type family Streams spec :: *

type instance Streams (TopicAs (f :: Format) (s :: Symbol) a) =
     Streamly.Serial a

type instance Streams (a :<|> b) = Streams a :<|> Streams b

class (PubSubTransport transport) =>
      Publisher spec transport
  where
  publish :: Proxy spec -> transport -> Streams spec -> IO ()

instance (Publisher a transport, Publisher b transport) =>
         Publisher (a
                    :<|> b) transport where
  publish ::
       Proxy (a
              :<|> b)
    -> transport
    -> (Streams a
        :<|> Streams b)
    -> IO ()
  publish _ transport (aStreams :<|> bStreams) = do
    publish (Proxy :: Proxy a) transport aStreams
    publish (Proxy :: Proxy b) transport bStreams

instance (KnownSymbol route, Show a, PubSubTransport transport) =>
         Publisher (Topic (route :: Symbol) a) transport where
  publish :: Proxy (Topic route a) -> transport -> Streamly.Serial a -> IO ()
  publish _ = _publish show (Route $ proxyText $ (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON a, PubSubTransport transport) =>
         Publisher (TopicJSON (route :: Symbol) a) transport where
  publish ::
       Proxy (TopicJSON route a) -> transport -> Streamly.Serial a -> IO ()
  publish _ = _publish encode (Route $ proxyText $ (Proxy :: Proxy route))

type family SubscriberBindings spec :: *

type instance
     SubscriberBindings (TopicAs (f :: Format) (s :: Symbol) a) =
     IO (Id, Streamly.Serial a)

type instance SubscriberBindings (a :<|> b) =
     SubscriberBindings a :<|> SubscriberBindings b

class (PubSubTransport transport) =>
      Subscriber spec transport
  where
  makeSubscriber :: Proxy spec -> transport -> SubscriberBindings spec

instance (Subscriber a transport, Subscriber b transport) =>
         Subscriber (a
                     :<|> b) transport where
  makeSubscriber ::
       Proxy (a
              :<|> b)
    -> transport
    -> SubscriberBindings a
       :<|> SubscriberBindings b
  makeSubscriber _ transport =
    makeSubscriber (Proxy :: Proxy a) transport :<|>
    makeSubscriber (Proxy :: Proxy b) transport

instance (KnownSymbol route, Read a, PubSubTransport transport) =>
         Subscriber (Topic (route :: Symbol) a) transport where
  makeSubscriber ::
       Proxy (Topic route a) -> transport -> IO (Id, Streamly.Serial a)
  makeSubscriber _ =
    _subscribe readUnsafe (Route $ proxyText $ (Proxy :: Proxy route))

instance (KnownSymbol route, FromJSON a, PubSubTransport transport) =>
         Subscriber (TopicJSON (route :: Symbol) a) transport where
  makeSubscriber ::
       Proxy (TopicJSON route a) -> transport -> IO (Id, Streamly.Serial a)
  makeSubscriber _ =
    _subscribe decodeUnsafe (Route $ proxyText $ (Proxy :: Proxy route))
