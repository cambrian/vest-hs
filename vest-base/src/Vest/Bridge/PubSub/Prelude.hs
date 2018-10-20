module Vest.Bridge.PubSub.Prelude
  ( module Vest.Bridge.PubSub.Prelude
  ) where

import Vest.Prelude

-- ^ The most recent <history size> messages should be delivered on _subscribe
-- TODO: improve interface, and possibly remove the Word32 param
class PubSubTransport t where
  _publish ::
       ((Text' "a" -> IO ()) -> IO ())
      -- ^ Publish fn, given a function to send a serialized @a over @t.
    -> Word32 -- ^ size of message history
    -> NamespacedText' "TopicName"
    -> t -- ^ Should be mutated to store cleanup details.
    -> IO ()
  _subscribe ::
       (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> Word32 -- ^ size of message history. Only used to create the AMQP exchange if it doesn't already exist
    -> NamespacedText' "TopicName"
    -> t -- ^ Should be mutated to store cleanup details.
    -> IO (IO' "Unsubscribe" ())

class (PubSubTransport transport) =>
      HasPubSubTransport transport t
  where
  pubSubTransport :: t -> transport

data EventOrValue
  = Event
  | Value

data Topic_ serializationFormat service transport (name :: k) (topictype :: EventOrValue) a

type Topic = Topic_ "Haskell"
