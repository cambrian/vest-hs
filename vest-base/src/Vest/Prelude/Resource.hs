module Vest.Prelude.Resource
  ( module Vest.Prelude.Resource
  ) where

import Data.Pool
import Data.Pool as Vest.Prelude.Resource (Pool, tryWithResource, withResource)
import Vest.Prelude.Core
import Vest.Prelude.Time

class Resource a where
  type ResourceConfig a
  make :: ResourceConfig a -> IO a
  cleanup :: a -> IO ()
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config = bracket (make config) cleanup
  -- ^ TODO: Retry on exception.

data PoolConfig a = PoolConfig
  { idleTime :: Time Second
  , numResources :: Word
  -- ^ numResources is technically per-stripe, but we just use 1 stripe.
  , resourceConfig :: ResourceConfig a
  }

-- Get a resource from the pool with @withResource
instance Resource a => Resource (Pool a) where
  type ResourceConfig (Pool a) = PoolConfig a
  make PoolConfig {idleTime, numResources, resourceConfig} =
    createPool
      (make resourceConfig)
      cleanup
      1
      (nominalDiffTimeFromTime idleTime)
      (fromIntegral numResources)
  cleanup = destroyAllResources

-- TODO: do resource acquisision in parallel
-- TODO: replace these fns with :<|> operator
with2 ::
     (Resource a, Resource b)
  => ResourceConfig a
  -> ResourceConfig b
  -> ((a, b) -> IO t)
  -> IO t
with2 ra rb f = with ra (\a -> with rb (\b -> f (a, b)))

with3 ::
     (Resource a, Resource b, Resource c)
  => ResourceConfig a
  -> ResourceConfig b
  -> ResourceConfig c
  -> ((a, b, c) -> IO t)
  -> IO t
with3 ra rb rc f = with ra (\a -> with rb (\b -> with rc (\c -> f (a, b, c))))

with4 ::
     (Resource a, Resource b, Resource c, Resource d)
  => ResourceConfig a
  -> ResourceConfig b
  -> ResourceConfig c
  -> ResourceConfig d
  -> ((a, b, c, d) -> IO t)
  -> IO t
with4 ra rb rc rd f =
  with ra (\a -> with rb (\b -> with rc (\c -> with rd (\d -> f (a, b, c, d)))))
-- ^ TODO: If you know a more generalized way to make these...
