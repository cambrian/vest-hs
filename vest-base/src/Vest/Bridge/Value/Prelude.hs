module Vest.Bridge.Value.Prelude
  ( module Vest.Bridge.Value.Prelude
  ) where

import Vest.Prelude

type ValueName = Text' "ValueName"

class ValueTransport t where
  publishValue ::
       t -- ^ Should be mutated to store cleanup details.
    -> ValueName
    -> IO (Text' "a" -> IO ())
    -- ^ Returns publish fn for this topic.
  subscribeValue ::
       t -- ^ Should be mutated to store cleanup details.
    -> ValueName
    -> (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> IO () -- ^ Expects the most recent value to be delivered on subscription.

data ValueTopic_ (fmt :: SerializationFormat) service transport (name :: k) a

type ValueTopic = ValueTopic_ 'Haskell
