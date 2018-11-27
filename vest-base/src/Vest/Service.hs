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

-- | We tried an implementation where we wouldn't require redisconnection for services that only
-- serve RPCs, but decided that the additional complexity isn't worth it.
class ( HasNamespace a
      , Server a (RpcSpec a)
      , Publisher a (ValueSpec a)
      , Producer a (EventsProduced a)
      , Consumer a (EventsConsumed a)
      , Has RedisConnection a
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
  run :: [Path Abs Dir] -> IO Void
  run paths =
    init @a paths $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) $ rpcHandlers a
      void . async . withDistributedLock a (Tagged (namespace @a) <> "-events") $ do
        publish a (Proxy :: Proxy (ValueSpec a)) $ valuesPublished a
        produce a (Proxy :: Proxy (EventsProduced a)) $ eventProducers a
        consume a (Proxy :: Proxy (EventsConsumed a)) $ eventConsumers a
      log_ Debug "service setup completed, now running forever"
      blockForever
  start :: IO Void
  start = do
    Args {configDir} <- cmdArgs $ args @a
    paths <- configPaths @a configDir
    run @a paths

-- | TODO: move (and rename?)
newtype Specific s a = Specific
  { base :: a
  }

instance (HasNamespace s, Resource a) => Resource (Specific s a) where
  type ResourceConfig (Specific s a) = ResourceConfig a
  makeLogged cfg = Specific <$> makeLogged cfg
  cleanupLogged = cleanupLogged . base
  resourceName = namespace @s <> "-" <> resourceName @a

instance (HasNamespace s, Loadable a) => Loadable (Specific s a) where
  configFile = namespaceDir @s </> configFile @a
