-- Class for resources that can be loaded from configs.
--
-- To define loadable data or resources
--
-- data A
--
-- instance Loadable A where
--   configName = [relfile|a|]
--
-- To use:
--
-- withLoadable configDirs $ \(a :<|> b) -> do
--   ...
--
-- For our loadable A, this will look for "a.yaml" within configDirs.
--
-- Currently configDirs must be all relative paths or all absolute paths. I'm debating making this
-- less type safe and simply allowing [FilePath] instead.
module Vest.Prelude.Loadable
  ( Loadable(..)
  , LoadableData(..)
  , LoadableResource(..)
  ) where

import Data.Pool
import Vest.Prelude.Core
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

instance {-# OVERLAPPABLE #-} (Loadable a, FromJSON a) => LoadableData a where
  load paths = configPathUnsafe @a paths >>= readYamlFile

instance (LoadableData a, LoadableData b) =>
         LoadableData (a
                       :<|> b) where
  load paths = do
    aThread <- async $ load @a paths
    bThread <- async $ load @b paths
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b

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
    aThread <- async $ makeLoadable @a paths
    bThread <- async $ makeLoadable @b paths
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b
