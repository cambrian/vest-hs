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
       t -- ^ Should be mutated to store cleanup details.
    -> RawTopicName
    -> IO (Text' "a" -> IO ())
    -- ^ Returns publish fn for this topic.
  subscribe_ ::
       t -- ^ Should be mutated to store cleanup details.
    -> RawTopicName
    -> (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> IO () -- ^ Expects message history to be delivered on subscription.

class (PubSubTransport transport) =>
      HasPubSubTransport transport t
  where
  pubSubTransport :: t -> transport

data Topic_ serializationFormat (topictype :: TopicType) service transport (name :: k) a

type Topic = Topic_ "Haskell"
