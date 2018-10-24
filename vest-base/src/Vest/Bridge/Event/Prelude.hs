module Vest.Bridge.Event.Prelude
  ( module Vest.Bridge.Event.Prelude
  ) where

import Vest.Bridge.Rpc
import Vest.Bridge.Variable
import Vest.Prelude

type PrefixedEventName (eventName :: Symbol) = AppendSymbol "event/" eventName

type family EventMaterializeEndpoint event where
  EventMaterializeEndpoint (Event_ fmt t rpcTransport _ name a) = Endpoint_ 5 fmt 'NoAuth t rpcTransport (PrefixedEventName name) (IndexOf a) ('Direct a)

type family EventVariable event where
  EventVariable (Event_ fmt t _ varTransport name a) = Variable_ fmt t varTransport (PrefixedEventName name) a

type EventName = Text' "EventName"

data Event_ (fmt :: SerializationFormat) service rpcTransport varTransport (name :: k) a

type Event = Event_ 'Haskell

class (HasRpcTransport rpcTransport t, HasVariableTransport varTransport t) =>
      HasEventTransport rpcTransport varTransport t


instance (HasRpcTransport rpcTransport t, HasVariableTransport varTransport t) =>
         HasEventTransport rpcTransport varTransport t

class (KnownSymbol s, KnownSymbol (PrefixedEventName s)) =>
      KnownEventName s


instance (KnownSymbol s, KnownSymbol (PrefixedEventName s)) =>
         KnownEventName s
