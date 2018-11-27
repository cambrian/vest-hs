module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs hiding (args, summary)
import qualified System.Console.CmdArgs as CmdArgs
import Vest.Bridge
import Vest.DistributedLock
import Vest.Prelude hiding (takeWhile)
import Vest.Redis

configPaths ::
     forall a. HasNamespace a
  => FilePath
  -> IO [Path Abs Dir]
-- ^ Will throw if configDir is not well formed.
configPaths configDir = do
  d <- resolveDir' configDir
  return [d </> namespaceDir @a, d]

-- | All services take only the config directory; any service-specific configuration can be done via
-- config files. A service will look in configDir/namespace/ first, then configDir/ for its configs.
-- Configs containing privileged info should be git-crypted.
-- TODO: add service-wide lock for consuming/publishing
data Args = Args -- This must be a data and not a newtype for cmdargs &= annotations to work.
  { configDir :: FilePath
  } deriving (Data)

class ( HasNamespace a
      , Server a (RpcSpec a)
      , Publisher a (ValueSpec a)
      , Producer a (EventsProduced a)
      , Consumer a (EventsConsumed a)
      ) =>
      Service a
  where
  type RpcSpec a
  type ValueSpec a
  type EventsProduced a
  type EventsConsumed a
  exe :: Text
  -- ^ Executable name for this service.
  exe = namespace @a
  summary :: Text
  -- ^ Displayed when user passes --version or --help.
  description :: Text
  -- ^ Displayed when user passes --help.
  init :: [Path Abs Dir] -> (a -> IO b) -> IO b
  rpcHandlers :: a -> Handlers (RpcSpec a)
  valuesPublished :: a -> Values (ValueSpec a)
  eventProducers :: a -> Producers (EventsProduced a)
  eventConsumers :: a -> Consumers (EventsConsumed a)
  args :: Args
  args =
    Args
      { configDir =
          "services/config/local" &= help "Config directory" &= explicit &=
          CmdArgs.name "config" &=
          CmdArgs.name "c" &=
          typDir
      } &=
    program (unpack $ exe @a) &=
    CmdArgs.summary (unpack $ summary @a) &=
    help (unpack $ description @a)

-- | Services need to acquire a service-wide lock if they publish or consume anything. Otherwise
-- any service instance may serve RPCs.
data AcquiresServiceLock
  = AcquireServiceLock
  | NoServiceLock

class Service a =>
      Service_ (lock :: AcquiresServiceLock) a
  where
  run_ :: [Path Abs Dir] -> (a -> IO b) -> IO Void
  start_ :: IO Void
  start_ = do
    Args {configDir} <- cmdArgs $ args @a
    paths <- configPaths @a configDir
    run_ @lock @a paths return

instance Service a => Service_ 'NoServiceLock a where
  run_ paths f =
    init paths $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) $ rpcHandlers a
      -- TODO: also include materializer RPC?
      f a
      log_ Debug "service setup completed, now running forever"
      blockForever

instance (Service a, Has RedisConnection a) =>
         Service_ 'AcquireServiceLock a where
  run_ paths f =
    run_ @'NoServiceLock @a paths $ \a -> do
      void . async . withDistributedLock a (Tagged (namespace @a) <> "-events") $ do
        publish a (Proxy :: Proxy (ValueSpec a)) $ valuesPublished a
        produce a (Proxy :: Proxy (EventsProduced a)) $ eventProducers a
        consume a (Proxy :: Proxy (EventsConsumed a)) $ eventConsumers a
      f a

start ::
     forall a. Service_ 'AcquireServiceLock a
  => IO Void
-- ^ This alias is provided to encourage service implementations to not forget to do service-locked
-- things.
start = start_ @'AcquireServiceLock @a

-- | TODO: move (and rename?)
newtype Specific s a = Specific
  { base :: a
  }

instance (HasNamespace s, Resource a) => Resource (Specific s a) where
  type ResourceConfig (Specific s a) = ResourceConfig a
  makeLogged = makeLogged
  cleanupLogged = cleanupLogged
  resourceName = namespace @s <> "-" <> resourceName @a

instance (HasNamespace s, Loadable a) => Loadable (Specific s a) where
  configFile = namespaceDir @s </> configFile @a
