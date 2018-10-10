module Service where

import Bridge
import Vest.Prelude

-- | TODO: Can we put shared argument logic here, like reading secret key files?
-- makeStreams and handlers are not defined on the Service because they cause circular imports :(
class ( Data (ServiceArgs a)
      , Typeable a
      , Server a (RpcSpec a)
      , Publisher a (PubSubSpec a)
      ) =>
      Service a
  where
  type ServiceArgs a -- Can't be data because cmdArgs has to get the type name.
  type PubSubSpec a
  type RpcSpec a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  -- ^ rename?
  --
  -- End of minimal required definition.
  --
  serviceName :: Text' "ServiceName"
  serviceName = moduleName' @a
  start :: (a -> IO (Streams (PubSubSpec a))) -> Handlers (RpcSpec a) -> IO Void
  start makeStreams handlers = startAndRun @a makeStreams handlers return
  startAndRun ::
       (a -> IO (Streams (PubSubSpec a)))
    -> Handlers (RpcSpec a)
    -> (a -> IO b)
    -> IO Void
  -- ^ This function allows you to start a service and run an arbitrary function in addition to
  -- serving and publishing according to spec.
  startAndRun makeStreams handlers f = do
    args_ <- cmdArgs $ defaultArgs @a
    init args_ $ \a -> do
      serve handlers a (Proxy :: Proxy (RpcSpec a))
      streams <- makeStreams a
      publish streams a (Proxy :: Proxy (PubSubSpec a))
      f a
      blockForever

instance Service a => HasNamespace a where
  namespace = retag $ serviceName @a
