module Service where

import Bridge
import Vest.Prelude

-- TODO: can we put shared argument logic here, like reading secret key files?
class (Data (ServiceArgs a), Typeable a) =>
      Service a
  where
  type ServiceArgs a -- can't be data because cmdArgs has to get the type name
  type RpcApi a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  -- ^ This isn't the cleanest but I can't think of anything better for the time being.
  rpcHandlers :: Handlers (RpcApi a)
  serviceName :: Text' "ServiceName"
  serviceName = moduleName' @a
  start :: (a -> IO Void) -> IO Void
  start f = do
    args <- cmdArgs $ defaultArgs @a
    init args f

instance Service a => HasNamespace a where
  namespace = retag $ serviceName @a
