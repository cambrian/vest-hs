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

-- Returns (push, close, stream).
-- Push throws if called after close is bound.
-- Close does nothing on repeated binds.
-- Remembers the most recent item pushed. A new consumption from this stream will see the most
-- recently pushed item, even if it is already closed.
pushStream :: IO (a -> IO (), IO' "CloseStream" (), Stream a)
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

-- Creates a TVar that is updated with the latest value from as.
-- The TVar is Nothing until the first value is received.
makeStreamVar :: Stream a -> IO (TVar (Maybe a))
makeStreamVar as = do
  var <- newTVarIO Nothing
  async $ Stream.mapM_ (atomically . writeTVar var . Just) as
  return var

-- Creates a TVar that is updated with the latest value from as.
-- The TVar has an explicit initial value.
makeStreamVar' :: a -> Stream a -> IO (TVar a)
makeStreamVar' init as = do
  var <- newTVarIO init
  async $ Stream.mapM_ (atomically . writeTVar var) as
  return var
