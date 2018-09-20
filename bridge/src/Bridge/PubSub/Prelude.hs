module Bridge.PubSub.Prelude
  ( module Bridge.PubSub.Prelude
  ) where

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

data TopicAs (f :: Format) (s :: k) (a :: *)

type Topic = TopicAs 'Haskell

type TopicJSON = TopicAs 'JSON
