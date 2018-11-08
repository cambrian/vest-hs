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
  init ::
       ServiceArgs a
    -> (( a
        , Handlers (RpcSpec a)
        , Values (ValueSpec a)
        , Producers (EventsProduced a)
        , Consumers (EventsConsumed a)) -> IO b)
    -> IO b
  run :: ServiceArgs a -> (a -> IO b) -> IO Void
  -- ^ This function runs a service with an arbitrary body function.
  run args f =
    init args $ \(a, handlers, values, producers, consumers) -> do
      serve a (Proxy :: Proxy (RpcSpec a)) handlers
      publish a (Proxy :: Proxy (ValueSpec a)) values
      produce a (Proxy :: Proxy (EventsProduced a)) producers
      consume a (Proxy :: Proxy (EventsConsumed a)) consumers
      f a
      blockForever
  start :: IO Void
  start = do
    args_ <- cmdArgs $ defaultArgs @a
    run @a args_ return
