module Vest.Prelude.Resource
  ( module Vest.Prelude.Resource
  ) where

import Data.Pool
import Data.Pool as Vest.Prelude.Resource (Pool, tryWithResource, withResource)
import Vest.Prelude.Core
import Vest.Prelude.Log
import Vest.Prelude.Time
import Vest.Prelude.TypeLevel

class Resource a where
  type ResourceConfig a
  makeRaw :: ResourceConfig a -> IO a
  makeRaw = make
  cleanupRaw :: a -> IO ()
  cleanupRaw = cleanup
  resourceName :: Text
  default resourceName :: Typeable a =>
    Text
  resourceName = moduleName @a <> "." <> constructorName @a
  make :: ResourceConfig a -> IO a
  make cfg = do
    log Debug "resource acquiring" $ resourceName @a
    r <- makeRaw cfg
    log Debug "resource acquired" $ resourceName @a
    return r
  cleanup :: a -> IO ()
  cleanup a = do
    log Debug "resource releasing" $ resourceName @a
    cleanupRaw a
    log Debug "resource released" $ resourceName @a
  {-# MINIMAL makeRaw, cleanupRaw | make, cleanup #-}
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config = bracket (make config) cleanup
  -- ^ TODO: Retry on exception.

data PoolConfig a = PoolConfig
  { idleTime :: Duration
  , numResources :: Word
  -- ^ numResources is technically per-stripe, but we just use 1 stripe.
  , resourceConfig :: ResourceConfig a
  }

-- | Get a resource from a pool with @withResource
instance Resource a => Resource (Pool a) where
  type ResourceConfig (Pool a) = PoolConfig a
  resourceName = "Pool " <> resourceName @a
  make PoolConfig {idleTime, numResources, resourceConfig} =
    createPool
      (make resourceConfig)
      cleanup
      1
      idleTime
      (fromIntegral numResources)
  cleanup = destroyAllResources

instance (Resource a, Resource b) =>
         Resource (a
                   :<|> b) where
  type ResourceConfig (a
                       :<|> b) = (ResourceConfig a
                                  :<|> ResourceConfig b)
  resourceName = resourceName @a <> " :<|> " <> resourceName @b
  make (aConfig :<|> bConfig) = do
    aThread <- async $ make aConfig
    bThread <- async $ make bConfig
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b
  cleanup (a :<|> b) = do
    aThread <- async $ cleanup a
    bThread <- async $ cleanup b
    void $ waitBoth aThread bThread
