module Vest.Service
  ( module Vest.Service
  ) where

import System.Console.CmdArgs hiding (args, summary)
import qualified System.Console.CmdArgs as CmdArgs
import Vest.Bridge
import Vest.Prelude hiding (takeWhile)

-- | All services take only the config directory; any service-specific configuration can be done via
-- config files. A service will look in configDir/namespace/ first, then configDir/ for its configs.
-- Configs containing priveleged info should be git-crypted.
newtype Args = Args
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
  -- ^ executable name for this service
  exe = namespace @a
  summary :: Text
  -- ^ displayed when user passes --version or --help
  description :: Text
  -- ^ displayed when user passes --help
  init :: [Path Abs Dir] -> (a -> IO b) -> IO b
  rpcHandlers :: a -> Handlers (RpcSpec a)
  valuesPublished :: a -> Values (ValueSpec a)
  eventProducers :: a -> Producers (EventsProduced a)
  eventConsumers :: a -> Consumers (EventsConsumed a)
  serviceConfigDir :: Path Rel Dir
  serviceConfigDir = [reldir|$(namespace @a)|]
  configPaths :: FilePath -> IO [Path Abs Dir]
  -- ^ will throw if configDir is not well formed
  configPaths configDir = do
    d <- resolveDir' configDir
    return [d </> serviceConfigDir @a, d]
  run :: FilePath -> (a -> IO b) -> IO Void
  -- ^ This function runs a service with an arbitrary body function.
  run configDir f = do
    paths <- configPaths @a configDir
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
          name "config" &=
          name "c" &=
          typDir
      } &=
    program (unpack $ exe @a) &=
    CmdArgs.summary (unpack $ summary @a) &=
    help (unpack $ description @a)
  start :: IO Void
  start = do
    Args {configDir} <- cmdArgs $ args @a
    run @a configDir return
