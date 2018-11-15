-- Class for resources that can be loaded from configs.
-- To use:
--
-- withLoadable @(A :<|> B) configDir $ \(a :<|> b) -> do
--   ...
module Vest.Loadable
  ( Loadable(..)
  , LoadableData(..)
  , LoadableResource(..)
  ) where

import Data.Pool
import qualified Data.Yaml as Yaml
import Vest.Prelude

class Loadable a where
  configName :: Text
  configFile :: Text
  configFile = configName @a <> ".yaml"
  configPath :: FilePath -> FilePath
  configPath = (<> "/" <> convertString (configFile @a))

class LoadableData a where
  load :: FilePath -> IO a

instance (Loadable a, FromJSON a) => LoadableData a where
  load = Yaml.decodeFileThrow . configPath @a

class Resource a =>
      LoadableResource a
  where
  makeLoadable :: FilePath -> IO a
  withLoadable :: FilePath -> (a -> IO b) -> IO b
  withLoadable dir = bracket (makeLoadable dir) cleanupLogged

instance {-# OVERLAPPABLE #-} ( Loadable a
                              , Resource a
                              , FromJSON (ResourceConfig a)
                              ) =>
                              LoadableResource a where
  makeLoadable dir = do
    config <- Yaml.decodeFileThrow $ configPath @a dir
    makeLogged config

data LoadablePoolConfig = LoadablePoolConfig
  { numResources :: Word
  , idleTime :: Duration
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance (Loadable a, Resource a, FromJSON (ResourceConfig a)) =>
         LoadableResource (Pool a) where
  makeLoadable dir = do
    let poolPath = dir <> "/" <> convertString (configName @a) <> "-pool.yaml"
    LoadablePoolConfig {numResources, idleTime} <- Yaml.decodeFileThrow poolPath
    resourceConfig <- Yaml.decodeFileThrow $ configPath @a dir
    createPool
      (makeLogged resourceConfig)
      cleanupLogged
      1
      idleTime
      (fromIntegral numResources)

instance (LoadableResource a, LoadableResource b) =>
         LoadableResource (a
                           :<|> b) where
  makeLoadable dir = do
    aThread <- async $ makeLoadable @a dir
    bThread <- async $ makeLoadable @b dir
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b
