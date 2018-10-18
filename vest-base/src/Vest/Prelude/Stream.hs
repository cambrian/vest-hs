module Vest.Prelude.Stream
  ( module Vest.Prelude.Stream
  ) where

import qualified Streamly
import qualified Streamly.Prelude as Stream
import Vest.Prelude.Core

type Stream a = Streamly.Serial a

data StreamPushAfterCloseException =
  StreamPushAfterCloseException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

pushStream :: IO (a -> IO (), IO' "CloseStream" (), Stream a)
-- ^ Returns (push, close, stream).
-- Push throws if called after close is bound.
-- Close does nothing on repeated binds.
-- Remembers the most recent item pushed. A new consumption from this stream will see the most
-- recently pushed item, even if it is already closed.
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
  return (push, Tagged close, stream)

tmvarFromStream :: Stream a -> IO (TMVar a)
-- ^ Creates a TMVar that is updated with the latest value from as.
tmvarFromStream as = do
  var <- newEmptyTMVarIO
  async $ Stream.mapM_ (atomically . swapTMVar var) as
  return var

data StreamClosedBeforeFirstValueException =
  StreamClosedBeforeFirstValueException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

tvarFromStreamUnsafe :: Stream a -> IO (TVar a)
-- ^ Creates a TVar that is updated with the latest value from as.
-- Blocks until the first item is received.
-- Throws if the stream closes without sending any values.
-- NB: This function may or may not skip items after the first item if they are sent before it has
-- the chance to begin listening on the stream the second time. I'm not sure.
tvarFromStreamUnsafe as =
  Stream.head as >>= \case
    Nothing -> throw StreamClosedBeforeFirstValueException
    Just a -> do
      var <- newTVarIO a
      async $ Stream.mapM_ (atomically . writeTVar var) as
      return var
