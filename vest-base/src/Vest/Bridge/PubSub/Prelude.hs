module Vest.Bridge.PubSub.Prelude
  ( module Vest.Bridge.PubSub.Prelude
  ) where

import Vest.Prelude

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

data Topic serializationFormat service transport (name :: k) a
