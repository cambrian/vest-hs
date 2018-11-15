-- Class for resources that can be loaded from configs.
-- To use:
--
-- withLoadable @(A :<|> B) configDir $ \(a :<|> b) -> do
--   ...
module Vest.Loadable
  ( Loadable(..)
  ) where

import Data.Pool
import qualified Data.Yaml as Yaml
import Vest.Prelude

-- This can't be defined because GHC does not support constraints on associated types
-- instance {-# OVERLAPPABLE #-} FromJSON a => Resource a where
--   type ResourceConfig a = FilePath
--   make = Yaml.decodeFileThrow
--   cleanup _ = return ()
class Resource a =>
      Loadable a
  where
  configName :: Text
  makeLoadable :: FilePath -> IO a
  default makeLoadable :: FromJSON (ResourceConfig a) =>
    FilePath -> IO a
  makeLoadable dir = do
    let configFile = dir <> "/" <> convertString (configName @a) <> ".yaml"
    config <- Yaml.decodeFileThrow configFile
    makeLogged config
  withLoadable :: FilePath -> (a -> IO b) -> IO b
  withLoadable dir = bracket (makeLoadable dir) cleanupLogged

data LoadablePoolConfig = LoadablePoolConfig
  { numResources :: Word
  , idleTime :: Duration
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Loadable a => Loadable (Pool a) where
  configName = configName @a <> "-pool"
  makeLoadable dir = do
    let poolFile = dir <> "/" <> convertString (configName @a) <> "-pool.yaml"
    LoadablePoolConfig {numResources, idleTime} <- Yaml.decodeFileThrow poolFile
    createPool
      (makeLoadable dir)
      cleanupLogged
      1
      idleTime
      (fromIntegral numResources)

instance (Loadable a, Loadable b) =>
         Loadable (a
                   :<|> b) where
  configName = configName @a <> "-" <> configName @b
  makeLoadable dir = do
    aThread <- async $ makeLoadable @a dir
    bThread <- async $ makeLoadable @b dir
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b
