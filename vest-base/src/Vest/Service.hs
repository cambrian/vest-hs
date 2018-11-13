module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs as Vest.Service
import Vest.Bridge
import Vest.Logger
import Vest.Prelude hiding (takeWhile)

-- | TODO: Can we put shared argument logic here, like reading secret key files?
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
  rpcHandlers :: a -> Handlers (RpcSpec a)
  valuesPublished :: a -> Values (ValueSpec a)
  eventProducers :: a -> Producers (EventsProduced a)
  eventConsumers :: a -> Consumers (EventsConsumed a)
  run :: ServiceArgs a -> (a -> IO b) -> IO Void
  -- ^ This function runs a service with an arbitrary body function.
  run args f =
    init args $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) $ rpcHandlers a
      publish a (Proxy :: Proxy (ValueSpec a)) $ valuesPublished a
      produce a (Proxy :: Proxy (EventsProduced a)) $ eventProducers a
      consume a (Proxy :: Proxy (EventsConsumed a)) $ eventConsumers a
      f a
      blockForever
  start :: IO Void
  start = do
    args_ <- cmdArgs $ defaultArgs @a
    run @a args_ return
