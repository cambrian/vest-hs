module Bridge.PubSub.Prelude
  ( module Bridge.PubSub.Prelude
  ) where

import Bridge.Prelude
import qualified Streamly
import VestPrelude

-- SubscribeOne (non-streaming version) is deliberately unimplemented, because RabbitMQ does not
-- support message history. Candidate solutions are building a separate pub/sub system on Kafka
-- (or similar), or adding Cassandra to RabbitMQ. For now, you can use makeStreamVar in
-- conjunction with subscribe' to get one-off values from a published topic.
class PubSubTransport t where
  _publish ::
       ((Text' "a" -> IO ()) -> Streamly.Serial a -> IO ())
    -- ^ (send publish object text to wire -> stream of publish objects)
    -> Text' "TopicName"
    -- ^ route to send wire-message on
    -> Streamly.Serial a
    -> t
    -- ^ transport
    -> IO ()
  _subscribe ::
       IO (Text' "a" -> IO (), IO (), Streamly.Serial a)
    -- ^ (push subscribe object text, close, result stream)
    -> Text' "TopicName"
    -- ^ route to listen for wire-messages on
    -> t
    -- ^ transport (should be mutated to store cleanup details)
    -> IO (Text' "SubscriberId", Streamly.Serial a)
  -- ^ subscriber ID and result stream as tuple (ID, stream).
  unsubscribe :: t -> Text' "SubscriberId" -> IO ()

data Topic (f :: Format) (name :: k) (a :: *)
