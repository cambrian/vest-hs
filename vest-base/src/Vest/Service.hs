module Vest.Service
  ( module Vest.Service
  ) where

import Data.Text (takeWhile)
import System.Console.CmdArgs as Vest.Service
import Vest.Bridge
import Vest.Prelude hiding (takeWhile)

-- | TODO: Can we put shared argument logic here, like reading secret key files?
-- makeStreams and handlers are not defined on the Service because they cause circular imports :(
class ( Data (ServiceArgs a)
      , Typeable a
      , Server a (RpcSpec a)
      , Publisher a (VariableSpec a)
      , Producer a (EventSpec a)
      ) =>
      Service a
  where
  type ServiceArgs a -- Can't be data because cmdArgs has to get the type name.
  type RpcSpec a
  type VariableSpec a
  type EventSpec a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  -- ^ Rename?
  --
  -- ^ Minimal complete definition.
  --
  serviceName :: Text
  serviceName = takeWhile (/= '.') (moduleName @a)
  serviceName' :: forall t. Text' t
  serviceName' = Tagged $ serviceName @a
  run ::
       ServiceArgs a
    -> Handlers (RpcSpec a)
    -> (a -> IO (Variables (VariableSpec a)))
    -> (a -> IO (Producers (EventSpec a)))
    -> (a -> IO b)
    -> IO Void
  -- ^ This function runs a service with the provided streams, handlers, and body function.
  run args handlers makeVariables makeEventProducers f =
    init args $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) handlers
      variables <- makeVariables a
      publish a (Proxy :: Proxy (VariableSpec a)) variables
      eventProducers <- makeEventProducers a
      produce a (Proxy :: Proxy (EventSpec a)) eventProducers
      f a
      blockForever
  start ::
       Handlers (RpcSpec a)
    -> (a -> IO (Variables (VariableSpec a)))
    -> (a -> IO (Producers (EventSpec a)))
    -> IO Void
  start handlers makeVariables makeEventProducers = do
    args_ <- cmdArgs $ defaultArgs @a
    run @a args_ handlers makeVariables makeEventProducers return

instance Service a => HasNamespace a where
  namespace = serviceName @a
