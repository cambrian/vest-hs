module TMap
  ( module TMap
  ) where

import Control.Concurrent.STM
import qualified Control.Monad.Parallel as Parallel
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import Protolude hiding (Map, foldl', toList)
import StmContainers.Map as TMap hiding (Map)
import StmContainers.Map

type TMap = Map

foldlM' :: ((k, v) -> accum -> STM accum) -> accum -> TMap k v -> STM accum
foldlM' f accum0 = UnfoldlM.foldlM' (flip f) accum0 . unfoldlM

foldl' :: ((k, v) -> accum -> accum) -> accum -> TMap k v -> STM accum
foldl' f = foldlM' (\a accum -> return $ f a accum)

toList :: TMap k v -> STM [(k, v)]
toList = foldl' (:) []

values :: TMap k v -> STM [v]
values t = map snd <$> toList t

parallelFilterM ::
     (Eq k, Hashable k) => ((k, v) -> IO Bool) -> TMap k v -> IO ()
-- Not entirely atomic; allows updates to the map while f is executed. Items added during this time
-- are ignored.
parallelFilterM f t = do
  kvs <- atomically $ toList t
  krs <-
    Parallel.mapM
      (\a@(k, _) -> do
         r <- f a
         return (k, r))
      kvs
  atomically $ mapM_ (\(k, r) -> unless r $ delete k t) krs

parallelFilterValuesM ::
     (Eq k, Hashable k) => (v -> IO Bool) -> TMap k v -> IO ()
parallelFilterValuesM f = parallelFilterM $ f . snd

parallelMapM_ :: ((k, v) -> IO a) -> TMap k v -> IO ()
-- Not entirely atomic; allows updates to the map while f is executed. Items added during this time
-- are ignored.
parallelMapM_ f t = do
  kvs <- atomically $ toList t
  Parallel.mapM_ f kvs

parallelMapValuesM_ :: (v -> IO a) -> TMap k v -> IO ()
-- Not entirely atomic; allows updates to the map while f is executed. Items added during this time
-- are ignored.
parallelMapValuesM_ f = parallelMapM_ (f . snd)
