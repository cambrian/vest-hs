module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs hiding (args, summary)
import qualified System.Console.CmdArgs as CmdArgs
import Vest.Bridge
import Vest.Prelude hiding (takeWhile)

-- | All services take only the config directory; any service-specific configuration can be done via
-- config files. A service will look in configDir/namespace/ first, then configDir/ for its configs.
-- Configs containing privileged info should be git-crypted.
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
  configPaths :: FilePath -> IO [Path Abs Dir]
  -- ^ Will throw if configDir is not well formed.
  configPaths configDir = do
    d <- resolveDir' configDir
    return [d </> namespaceDir @a, d]
  run :: [Path Abs Dir] -> (a -> IO b) -> IO Void
  -- ^ This function runs a service with an arbitrary body function.
  run paths f =
    init paths $ \a -> do
      serve a (Proxy :: Proxy (RpcSpec a)) $ rpcHandlers a
      publish a (Proxy :: Proxy (ValueSpec a)) $ valuesPublished a
      produce a (Proxy :: Proxy (EventsProduced a)) $ eventProducers a
      consume a (Proxy :: Proxy (EventsConsumed a)) $ eventConsumers a
      f a
      log_ Debug "service setup completed, now running forever"
      blockForever
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
  start :: IO Void
  start = do
    Args {configDir} <- cmdArgs $ args @a
    paths <- configPaths @a configDir
    run @a paths return

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
