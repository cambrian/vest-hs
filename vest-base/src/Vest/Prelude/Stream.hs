module Vest.Prelude.Stream
  ( module Vest.Prelude.Stream
  ) where

import qualified Data.PQueue.Prio.Min as PQueue
import qualified Streamly
import qualified Streamly.Prelude as Stream
import Vest.Prelude.Core

type Stream a = Streamly.Serial a

data StreamPushAfterCloseException =
  StreamPushAfterCloseException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

pushStream :: IO (a -> IO (), IO (), Stream a, STM a)
-- ^ Returns (push, close, stream, peek).
-- Push throws if called after close is bound.
-- Close does nothing on repeated binds.
-- Remembers the most recent item pushed. A new consumption from this stream will see the most
-- recently pushed item, even if it is already closed.
-- Peek returns the most recent value.
-- TODO: rewrite this as a newtype / data? (TStream). Would allow us to use STM instead of IO in
-- a more convenient way
pushStream = do
  t <- newTVarIO (Nothing, False, 0 :: Word64) -- Stores (Maybe value, closed, counter).
  let push a =
        atomically $ do
          (_, closed, counter) <- readTVar t
          when closed (throwSTM StreamPushAfterCloseException)
          writeTVar t (Just a, False, counter + 1)
      close =
        atomically $ modifyTVar' t (\(x, _, counter) -> (x, True, counter))
      stream =
        Stream.unfoldrM
          (\highestSeen ->
             atomically $ do
               (value, closed, counter) <- readTVar t
               unless (closed || counter > highestSeen) retry
               if counter > highestSeen
                 then case value of
                        Just x -> return $ Just (x, counter)
                        Nothing -> throwSTM BugException
                 else return Nothing)
          0
      peek =
        readTVar t >>= \case
          (Nothing, _, _) -> retry
          (Just v, _, _) -> return v
  return (push, close, stream, peek)

-- | Prepends historical values to an indexable stream and runs an action for each value.
observeFromIndex ::
     (Indexable res)
  => Stream res
  -> (IndexOf res -> IO res)
  -> (res -> IO ())
  -> IndexOf res
  -> IO ()
observeFromIndex original materialize pushResult startIndex = do
  resultQueueVar <- newTVarIO PQueue.empty
  firstResultSeen <- newEmptyTMVarIO
  -- Queue original elements.
  originalThread <-
    async $
    Stream.mapM_
      (\res ->
         atomically $ do
           modifyTVar resultQueueVar (PQueue.insert (index res) res)
           tryPutTMVar firstResultSeen (index res))
      original
  -- Queue materialize elements.
  materializeThread <-
    async $ do
      firstSeenIndex <- atomically $ readTMVar firstResultSeen
      mapM_
        (\backIndex -> do
           res <- materialize backIndex
           -- ^ TODO: Retry on failure?
           atomically $
             modifyTVar resultQueueVar (PQueue.insert (index res) res))
        [startIndex .. pred firstSeenIndex]
  -- Push output from queue monotonically.
  let pushFrom index = do
        nextResMaybe <-
          atomically $
          runMaybeT $ do
            resultQueue <- lift $ readTVar resultQueueVar
            let minKeyValue = PQueue.getMin resultQueue
            lift $ check (isJust minKeyValue)
            -- ^ Retry until the queue is non-empty.
            (minIndex, res) <- MaybeT . return $ minKeyValue
            lift $ check (minIndex == index)
            -- ^ Retry until the next index is what we expect.
            lift $ modifyTVar resultQueueVar PQueue.deleteMin
            return res
        nextRes <- fromJustUnsafe BugException nextResMaybe
        -- ^ STM retries ensure that res should never be Nothing.
        pushResult nextRes
        pushFrom (succ index)
  pushThread <- async $ pushFrom startIndex
  -- Kill pushThread when producers are done and there is nothing left to push.
  void . async $ do
    wait originalThread
    wait materializeThread
    atomically $ do
      resultQueue <- readTVar resultQueueVar
      check (PQueue.size resultQueue == 0)
    cancel pushThread
  -- Block on pushThread so clients can manually make this async.
  void $ waitCatch pushThread
