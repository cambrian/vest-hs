module Vest.Prelude.Resource
  ( module Vest.Prelude.Resource
  ) where

import Data.Pool
import Data.Pool as Vest.Prelude.Resource (Pool, tryWithResource, withResource)
import Vest.Prelude.Core
import Vest.Prelude.Time

data PoolConfig = PoolConfig
  { idleTime :: Time Second
  , numResources :: Word
  -- numResources is technically per-stripe, but we just use 1 stripe.
  }

class Resource a where
  type ResourceConfig a
  make :: ResourceConfig a -> IO a
  cleanup :: a -> IO ()
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config = bracket (make config) cleanup
  -- ^ TODO: Retry on exception.
  withPool :: PoolConfig -> ResourceConfig a -> (Pool a -> IO b) -> IO b
  -- ^ Use: withPool poolcfg cfg (\pool -> withResource pool (\resource -> do ...))
  withPool PoolConfig {idleTime, numResources} config =
    bracket
      (createPool
         (make config)
         cleanup
         1
         (nominalDiffTimeFromTime idleTime)
         (fromIntegral numResources))
      destroyAllResources
