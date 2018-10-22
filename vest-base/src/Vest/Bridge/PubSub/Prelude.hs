module Vest.Bridge.PubSub.Prelude
  ( module Vest.Bridge.PubSub.Prelude
  ) where

import Vest.Prelude

data TopicType
  = Event
  | Value

type RawTopicName = Text' "RawTopicName"

newtype AlreadyPublishingException =
  AlreadyPublishing RawTopicName
  deriving (Eq, Show, Read, Generic)
  deriving anyclass (Exception, FromJSON, ToJSON)

-- TODO: rename methods
-- TODO: split into event/value transports?
class PubSubTransport t where
  initTopic :: t -> RawTopicName -> TopicType -> IO ()
  initPublisher ::
       RawTopicName
    -> t -- ^ Should be mutated to store cleanup details.
    -> IO (Text' "a" -> IO ())
    -- ^ Returns publish fn for this topic.
  subscribe_ ::
       RawTopicName
    -> t -- ^ Should be mutated to store cleanup details.
    -> (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> IO () -- ^ Expects message history to be delivered on subscription.

class (PubSubTransport transport) =>
      HasPubSubTransport transport t
  where
  pubSubTransport :: t -> transport

data Topic_ serializationFormat (topictype :: TopicType) service transport (name :: k) a

type Topic = Topic_ "Haskell"

class (Ord (IndexOf a), Enum (IndexOf a)) =>
      Indexable a
  where
  type IndexOf a
  index :: a -> IndexOf a

instance (Ord (IndexOf a), Enum (IndexOf a)) => Indexable a where
  type IndexOf a = a
  index = identity
