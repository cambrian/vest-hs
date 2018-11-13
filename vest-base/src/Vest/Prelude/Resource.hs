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
  make :: ResourceConfig a -> IO a
  cleanup :: a -> IO ()
  resourceName :: Text
  default resourceName :: Typeable a =>
    Text
  resourceName = moduleName @a <> "." <> constructorName @a
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config =
    bracket
      (do log Debug "Resource acquiring" $ resourceName @a
          r <- make config
          log Debug "Resource acquired" $ resourceName @a
          return r)
      (\r -> do
         log Debug "Resource releasing" $ resourceName @a
         cleanup r
         log Debug "Resource released" $ resourceName @a)
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
