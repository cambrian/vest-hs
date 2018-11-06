module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs as Vest.Service
import Vest.Bridge
import Vest.Logger
import Vest.Prelude hiding (takeWhile)

-- | TODO: Can we put shared argument logic here, like reading secret key files?
-- makeStreams and handlers are not defined on the Service because they cause circular imports :(
class ( Data (ServiceArgs a)
      , HasNamespace a
      , HasLogger a
      , Server a (RpcSpec a)
      , Publisher a (ValueSpec a)
      , Producer a (EventsProduced a)
      , Consumer a (EventsConsumed a)
      ) =>
      Service a
  where
  type ServiceArgs a
  type RpcSpec a
  type ValueSpec a
  type EventsProduced a
  type EventsConsumed a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  run ::
       ServiceArgs a
    -> Handlers (RpcSpec a)
    -> (a -> IO (Values (ValueSpec a)))
    -> (a -> IO (Producers (EventsProduced a)))
    -> (a -> Consumers (EventsConsumed a))
    -> (a -> IO b)
    -> IO Void
  -- ^ This function runs a service with the provided streams, handlers, and body function.
  run args handlers makeValues makeEventProducers makeEventConsumers f =
    init args $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) handlers
      variables <- makeValues a
      publish a (Proxy :: Proxy (ValueSpec a)) variables
      eventProducers <- makeEventProducers a
      produce a (Proxy :: Proxy (EventsProduced a)) eventProducers
      consume a (Proxy :: Proxy (EventsConsumed a)) $ makeEventConsumers a
      f a
      blockForever
  start ::
       Handlers (RpcSpec a)
    -> (a -> IO (Values (ValueSpec a)))
    -> (a -> IO (Producers (EventsProduced a)))
    -> (a -> Consumers (EventsConsumed a))
    -> IO Void
  start handlers makeValues makeEventProducers makeEventConsumers = do
    args_ <- cmdArgs $ defaultArgs @a
    run
      @a
      args_
      handlers
      makeValues
      makeEventProducers
      makeEventConsumers
      return
