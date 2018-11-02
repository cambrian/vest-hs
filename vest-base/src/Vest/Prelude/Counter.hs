-- Provides a global counter, similar to the global random generator.
-- For testing, mostly.
-- TODO: Consider removing from Vest.Prelude.
module Vest.Prelude.Counter
  ( setGlobalCounter
  , nextCount
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Vest.Prelude.Core

{-# NOINLINE globalCounter #-}
globalCounter :: TVar Int64
globalCounter = unsafePerformIO $ newTVarIO 0

setGlobalCounter :: Int64 -> IO ()
setGlobalCounter = atomically . writeTVar globalCounter

nextCount :: IO Int64
-- nextCount = atomically $ stateTVar globalCounter $ \x -> (x, x + 1)
-- stateTVar is not available until stm-2.5
nextCount =
  atomically $ do
    count <- readTVar globalCounter
    writeTVar globalCounter $ count + 1
    return count
