module Service where

import Bridge
import Vest.Prelude

-- TODO: can we put shared argument logic here, like reading secret key files?
class ( Data (ServiceArgs a)
      , Typeable a
      , Server a (RpcSpec a)
      , Publisher a (PubSubSpec a)
      ) =>
      Service a
  where
  type ServiceArgs a -- can't be data because cmdArgs has to get the type name
  type PubSubSpec a
  type RpcSpec a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  -- ^ rename?
  rpcHandlers :: Handlers (RpcSpec a)
  makePublishStreams :: a -> IO (Streams (PubSubSpec a))
  --
  -- End of minimal required definition.
  --
  serviceName :: Text' "ServiceName"
  serviceName = moduleName' @a
  start :: IO Void
  start = startAndRun @a return
  startAndRun :: (a -> IO b) -> IO Void
  -- ^ This function allows you to start a service and run an arbitrary function in addition to
  -- serving and publishing according to spec.
  startAndRun f = do
    args <- cmdArgs $ defaultArgs @a
    init args $ \a -> do
      serve (rpcHandlers @a) a (Proxy :: Proxy (RpcSpec a))
      streams <- makePublishStreams a
      publish streams a (Proxy :: Proxy (PubSubSpec a))
      f a
      blockForever

instance Service a => HasNamespace a where
  namespace = retag $ serviceName @a
