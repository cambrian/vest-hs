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

pushStream :: IO (a -> IO (), IO (), Stream a, STM a)
-- ^ Returns (push, close, stream, readStream).
-- Push throws if called after close is bound.
-- Close does nothing on repeated binds.
-- Remembers the most recent item pushed. A new consumption from this stream will see the most
-- recently pushed item, even if it is already closed.
-- readStream returns the most recent value.
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
      readStream =
        readTVar t >>= \case
          (Nothing, _, _) -> retry
          (Just v, _, _) -> return v
  return (push, close, stream, readStream)
