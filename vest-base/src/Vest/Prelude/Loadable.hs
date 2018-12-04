-- Class for resources that can be loaded from configs.
--
-- To define loadable data or resources
--
-- data A
--
-- instance Loadable A where
--   configName = [relfile|a.yaml|]
--
-- To use:
--
-- withLoadable configDirs $ \(a :<|> b) -> do
--   ...
--
-- For our loadable A, this will look for "a.yaml" within configDirs.
module Vest.Prelude.Loadable
  ( Loadable(..)
  , LoadableData(..)
  , LoadableResource(..)
  , Specific(..)
  ) where

import Data.Pool
import Vest.Prelude.Core
import Vest.Prelude.Namespace
import Vest.Prelude.Resource
import Vest.Prelude.Time
import Vest.Prelude.TypeLevel

data MissingConfigException = MissingConfigException
  { configFile :: Path Rel File
  , searchedPaths :: [Path Abs Dir]
  } deriving (Eq, Show, Exception)

class Loadable a where
  configFile :: Path Rel File
  -- ^ TODO: it would be nice to take Text instead of Path Rel File, so long as it still can be
  -- checked at compile time.
  configPathUnsafe :: [Path Abs Dir] -> IO (Path Abs File)
  configPathUnsafe searchPaths =
    findFile searchPaths (configFile @a) >>=
    fromJustUnsafe (MissingConfigException (configFile @a) searchPaths)

class LoadableData a where
  load :: [Path Abs Dir] -> IO a

instance {-# OVERLAPPING #-} (LoadableData a, LoadableData b) =>
                             LoadableData (a
                                           :<|> b) where
  load paths = do
    aThread <- asyncDetached $ load @a paths
    bThread <- asyncDetached $ load @b paths
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b

instance {-# OVERLAPPABLE #-} (Loadable a, FromJSON a) => LoadableData a where
  load paths = configPathUnsafe @a paths >>= readYamlFile

class Resource a =>
      LoadableResource a
  where
  makeLoadable :: [Path Abs Dir] -> IO a
  withLoadable :: [Path Abs Dir] -> (a -> IO b) -> IO b
  withLoadable paths = bracket (makeLoadable paths) cleanupLogged

instance {-# OVERLAPPABLE #-} ( Loadable a
                              , Resource a
                              , FromJSON (ResourceConfig a)
                              ) =>
                              LoadableResource a where
  makeLoadable paths = do
    config <- configPathUnsafe @a paths >>= readYamlFile
    makeLogged config

data LoadablePoolConfig = LoadablePoolConfig
  { numResources :: Word
  , idleTime :: Duration
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance (Loadable a, Resource a, FromJSON (ResourceConfig a)) =>
         LoadableResource (Pool a) where
  makeLoadable paths = do
    path <- configPathUnsafe @a paths
    poolPath <- setFileExtension "pool.yaml" path
    resourceConfig <- readYamlFile path
    LoadablePoolConfig {numResources, idleTime} <- readYamlFile poolPath
    createPool
      (makeLogged resourceConfig)
      cleanupLogged
      1
      idleTime
      (fromIntegral numResources)

instance (LoadableResource a, LoadableResource b) =>
         LoadableResource (a
                           :<|> b) where
  makeLoadable paths = do
    aThread <- asyncDetached $ makeLoadable @a paths
    bThread <- asyncDetached $ makeLoadable @b paths
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b

-- | If you have a Loadable T, you also have a Loadable (Specific svc T)
-- where @load will look for servicedir/config-file.yaml instead of just config-file.yaml
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
