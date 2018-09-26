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
       ((Text -> IO ()) -> Streamly.Serial a -> IO ())
    -- ^ (send publish object text to wire -> stream of publish objects)
    -> Id "Topic"
    -- ^ route to send wire-message on
    -> t
    -- ^ transport
    -> Streamly.Serial a
    -> IO ()
  _subscribe ::
       IO (Text -> IO (), IO (), Streamly.Serial a)
    -- ^ (push subscribe object text, close, result stream)
    -> Id "Topic"
    -- ^ route to listen for wire-messages on
    -> t
    -- ^ transport (should be mutated to store cleanup details)
    -> IO (Id "Subscriber", Streamly.Serial a)
  -- ^ subscriber ID and result stream as tuple (ID, stream).
  unsubscribe :: t -> Id "Subscriber" -> IO ()

data Topic (f :: Format) (s :: k) (a :: *)
