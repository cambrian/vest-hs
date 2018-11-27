module Vest.Bridge.Event.Prelude
  ( module Vest.Bridge.Event.Prelude
  ) where

import Vest.Bridge.Rpc
import Vest.Prelude

type EventName = Text' "EventName"

-- | In theory the RPC and PubSub portions of the event harness could run on separate transports.
-- For this reason, EventTransport is its own class, and not a superclass of RpcTransport.
class EventTransport t where
  publishEvents ::
       t -- ^ Should be mutated to store cleanup details.
    -> EventName
    -> IO (Text' "a" -> IO ())
    -- ^ Returns publish fn for this topic.
  subscribeEvents ::
       t -- ^ Should be mutated to store cleanup details.
    -> EventName
    -> (Text' "a" -> IO ()) -- ^ Called per item received from the transport.
    -> IO () -- ^ Does not expect message history.

type PrefixedEventName (eventName :: Symbol) = AppendSymbol "event/" eventName

type family EventMaterializeEndpoint event where
  EventMaterializeEndpoint (Event_ fmt t transport name a) = Endpoint_ 5 fmt 'NoAuth t transport (PrefixedEventName name) (IndexOf a) ('Direct a)

-- | Even though we separate EventTransport and RpcTransport, the transport here has to be both
-- because we're lazy.
data Event_ (fmt :: SerializationFormat) service transport (name :: k) a

type Event = Event_ 'Haskell

class (KnownSymbol s, KnownSymbol (PrefixedEventName s)) =>
      KnownEventName s


instance (KnownSymbol s, KnownSymbol (PrefixedEventName s)) =>
         KnownEventName s
