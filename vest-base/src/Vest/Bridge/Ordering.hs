module Vest.Bridge.Ordering
  ( module Vest.Bridge.Ordering
  ) where

import qualified Data.PQueue.Prio.Min as PQueue
import qualified Stream
import Vest.Prelude

class Integral index =>
      Ordered res index
  where
  index :: res -> index

-- Back-fills a stream using a materializer.
-- Sus that the insert needs a type annotation.
backFill ::
     forall res index. (Ordered res index)
  => Stream res
  -> (index -> IO res)
  -> index
  -> IO (Stream res)
backFill original materialize startIndex = do
  (push, Tagged close, filledStream) <- pushStream
  resultQueueVar <- newTVarIO PQueue.empty
  waitForResults <- newEmptyTMVarIO
  -- Queue original stream elements.
  originalThread <-
    async $
    Stream.mapM_
      (\res ->
         atomically $ do
           modifyTVar resultQueueVar (PQueue.insert (index res :: index) res)
           tryPutTMVar waitForResults (index res))
      original
  -- Queue materialized elements.
  backFillThread <-
    async $ do
      firstSeenIndex <- atomically $ readTMVar waitForResults
      mapM_
        (\backIndex -> do
           res <- materialize backIndex
           atomically $ do
             modifyTVar resultQueueVar (PQueue.insert (index res :: index) res)
             tryPutTMVar waitForResults (index res))
        [startIndex .. firstSeenIndex - 1]
  -- Stream output from queue monotonically.
  let streamFrom index = do
        resMaybe <-
          atomically $
          runMaybeT $ do
            lift $ takeTMVar waitForResults
            resultQueue <- lift $ readTVar resultQueueVar
            (minIndex, res) <- MaybeT . return $ PQueue.getMin resultQueue
            guard (minIndex == index) -- Monotonic ordering condition.
            lift $ modifyTVar resultQueueVar PQueue.deleteMin
            return res
        case resMaybe of
          Just res -> push res >> streamFrom (index + 1)
          Nothing -> streamFrom index
  streamThread <- async $ streamFrom startIndex
  -- Close output stream when producers die.
  async $ do
    wait backFillThread
    wait originalThread
    cancel streamThread
    close
  return filledStream
