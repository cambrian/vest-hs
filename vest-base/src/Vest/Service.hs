module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs as Vest.Service
import Vest.Bridge
import Vest.Prelude

-- | TODO: Can we put shared argument logic here, like reading secret key files?
-- makeStreams and handlers are not defined on the Service because they cause circular imports :(
class ( Data (ServiceArgs a)
      , Typeable a
      , Server a (RpcSpec a)
      , Publisher a (PublishSpec a)
      ) =>
      Service a
  where
  type ServiceArgs a -- Can't be data because cmdArgs has to get the type name.
  type PublishSpec a
  type RpcSpec a
  defaultArgs :: ServiceArgs a
  init :: ServiceArgs a -> (a -> IO b) -> IO b
  -- ^ rename?
  serviceName :: Text' "ServiceName"
  serviceName = moduleName' @a
  run ::
       ServiceArgs a
    -> (a -> IO (Streams (PublishSpec a)))
    -> Handlers (RpcSpec a)
    -> (a -> IO b)
    -> IO Void
  -- ^ This function runs a service with the provided streams, handlers, and body function
  run args makeStreams handlers f =
    init args $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) handlers
      streams <- makeStreams a
      publish a (Proxy :: Proxy (PublishSpec a)) streams
      f a
      blockForever
  start ::
       (a -> IO (Streams (PublishSpec a))) -> Handlers (RpcSpec a) -> IO Void
  start makeStreams handlers = do
    args_ <- cmdArgs $ defaultArgs @a
    run @a args_ makeStreams handlers return

instance Service a => HasNamespace a where
  namespace = retag $ serviceName @a
