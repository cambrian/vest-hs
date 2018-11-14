module Vest.Prelude.Resource
  ( module Vest.Prelude.Resource
  ) where

import Data.Pool
import Data.Pool as Vest.Prelude.Resource (Pool, tryWithResource, withResource)
import Vest.Prelude.Core
import Vest.Prelude.Log
import Vest.Prelude.Time
import Vest.Prelude.TypeLevel

-- | A resource is any asset that must be cleaned up after use. Examples include file handles and
-- network connections.
-- Resources should be used with @with, although if you need to interface with other libraries
-- you might need to use @makeLogged or @cleanupLogged directly.
--
-- TODO: How to allow construction with @with, without making make/cleanup super ugly?
-- an idea: a global table of resource handles (weak pointers), and implement make/cleanup in terms
-- of @with
class Resource a where
  type ResourceConfig a
  make :: ResourceConfig a -> IO a
  make = makeLogged
  cleanup :: a -> IO ()
  cleanup = cleanupLogged
  makeLogged :: ResourceConfig a -> IO a
  makeLogged cfg = do
    log Debug "resource acquiring" $ resourceName @a
    r <- make cfg
    log Debug "resource acquired" $ resourceName @a
    return r
  cleanupLogged :: a -> IO ()
  cleanupLogged a = do
    log Debug "resource releasing" $ resourceName @a
    cleanup a
    log Debug "resource released" $ resourceName @a
  {-# MINIMAL make, cleanup | makeLogged, cleanupLogged #-}
  -- ^ defining @makeLogged and @cleanupLogged allows you to change the default logging behavior.
  -- Used below to skip logging where it would be cluttery.
  resourceName :: Text
  -- ^ For logging purposes
  default resourceName :: Typeable a =>
    Text
  resourceName = moduleName @a <> "." <> constructorName @a
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config = bracket (makeLogged config) cleanupLogged -- ^ TODO: Retry on exception?

-- | Require use of the logged versions of make/cleanup
{-# WARNING
make "use `with resourceConfig $ resource -> ...` if you can, otherwise use `makeLogged`"
 #-}

{-# WARNING
cleanup "use `with resourceConfig $ resource -> ...` if you can, otherwise use `cleanupLogged`"
 #-}

data PoolConfig a = PoolConfig
  { idleTime :: Duration
  , numResources :: Word
  -- ^ numResources is technically per-stripe, but we just use 1 stripe.
  , resourceConfig :: ResourceConfig a
  }

-- | Get a resource from a pool with @withResource.
instance Resource a => Resource (Pool a) where
  type ResourceConfig (Pool a) = PoolConfig a
  resourceName = "Pool " <> resourceName @a
  makeLogged PoolConfig {idleTime, numResources, resourceConfig} =
    createPool
      (makeLogged resourceConfig)
      cleanupLogged
      1
      idleTime
      (fromIntegral numResources)
  cleanupLogged = destroyAllResources

instance (Resource a, Resource b) =>
         Resource (a
                   :<|> b) where
  type ResourceConfig (a
                       :<|> b) = (ResourceConfig a
                                  :<|> ResourceConfig b)
  resourceName = resourceName @a <> " :<|> " <> resourceName @b
  makeLogged (aConfig :<|> bConfig) = do
    aThread <- async $ makeLogged aConfig
    bThread <- async $ makeLogged bConfig
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b
  cleanupLogged (a :<|> b) = do
    aThread <- async $ cleanupLogged a
    bThread <- async $ cleanupLogged b
    void $ waitBoth aThread bThread
