module Bridge.PubSub.Prelude
  ( module Bridge.PubSub.Prelude
  ) where

import Vest.Prelude

-- SubscribeOne (non-streaming version) is deliberately unimplemented, because
-- RabbitMQ does not support message history. Candidate solutions are building
-- a separate pub/sub system on Kafka (or similar), or adding Cassandra to
-- RabbitMQ. For now, you can use makeStreamVar in conjunction with subscribe'
-- to get one-off values from a published topic.
class PubSubTransport t where
  _publish ::
       ((Text' "a" -> IO ()) -> IO ())
      -- ^ Publish fn, given a function to send a serialized @a over @t.
    -> NamespacedText' "TopicName"
    -> t
    -> IO ()
  _subscribe ::
       (Text' "a" -> IO ()) -- ^ Fn to publish serialized object.
    -> NamespacedText' "TopicName"
    -> t -- ^ Should be mutated to store cleanup details.
    -> IO (IO' "Unsubscribe" ())

class (PubSubTransport transport) =>
      HasPubSubTransport transport t
  where
  pubSubTransport :: t -> transport

data Topic service (f :: SerializationFormat) transport (name :: k) a
