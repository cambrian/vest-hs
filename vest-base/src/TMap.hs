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

foldlM' :: (accum -> (k, v) -> STM accum) -> accum -> TMap k v -> STM accum
foldlM' f accum0 = UnfoldlM.foldlM' f accum0 . unfoldlM

foldl' :: (accum -> (k, v) -> accum) -> accum -> TMap k v -> STM accum
foldl' f = foldlM' (\accum a -> return $ f accum a)

toList :: TMap k v -> STM [(k, v)]
toList = foldl' (flip (:)) []

values :: TMap k v -> STM [v]
values t = fmap snd <$> toList t

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
