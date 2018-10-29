-- | Asynchronous streams with buffering and fanout, a la Async.Pipe for OCaml
--
-- Future improvements:
-- - merging. Streams will most likely have to maintain a list of pushers, and close when all of
--   their pushers close.
-- - track and compress the fanout tree.
--
-- OVERVIEW:
--
-- StreamPusher/Stream distiction:
-- Streams are split into their reading and writing ends; a StreamPusher is the writing end, and a
-- Stream is the reading end.
--
-- Buffering:
-- Each stream is backed by a buffer. Internally a child thread (propagator) attempts to unbuffer
-- items and push them downstream. The propagator waits for the pushes downstream to finish (in
-- parallel) before unbuffering the next item. This means that items may pile up inside
-- the buffer if pushes occur faster than the downstream consumption can handle.
--
-- pushStream:
-- - when closed, does nothing and returns False
-- - when not closed, blocks until the buffer is not full, adds the message, returns True.
--
-- Fanouts:
-- A fanout is a consumer of a stream's values. Internally, there are 2 types of fanouts:
-- downstream StreamPushers and taps. Downstreams represent streams chained via pure functions,
-- and taps represent effects to be run per value. Streams chained via impure functions are
-- implemented as specialized taps.
--
-- Garbage collection:
-- For memory/performance friendliness, streams will automatically close when they can no longer
-- produce values / effects. You should still manually close your streams, but automatic closing
-- should occur when:
-- - The StreamPusher goes out of scope.
-- - The Stream goes out of scope and has no fanouts.
module Vest.Prelude.Stream
  ( Bufferable
  , QueueBuffer_
  , QueueBuffer
  , ValueBuffer
  , StreamPusher(..)
  , pushStream
  , closeStream
  , Stream
  , newStream
  , filterMapMStream
  , filterMapStream
  , filterStream
  , mapMStream
  , mapStream
  , tapStream
  , tapStream_
  , tapFoldStream
  , waitStream
  , foldMStream
  , foldStream
  , consumeStream
  , listFromStream
  , streamFromList
  , readLatestValueSTM
  , readLatestValue
  , readFinalValue
  ) where

import Control.Concurrent.STM.TBQueue
import qualified Control.Monad.Parallel as Parallel
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Functor.Contravariant.Divisible
  ( Decidable(choose, lose)
  , Divisible(conquer, divide)
  )
import Data.Monoid (Monoid(mempty))
import Data.Semigroup
import qualified TMap
import Vest.Prelude.Core

-- | Backing storage for streams.
-- Closing is handled manually at the stream layer, so buffers don't need to worry about it.
class Bufferable t a where
  newBuf :: STM (t a)
  readBuf :: t a -> STM a
  writeBuf :: t a -> a -> STM ()
  isEmptyBuf :: t a -> STM Bool
  bufHistory :: t a -> STM [a] -- ^ Somewhat hacky. A way for value buffers to report history for new fanouts.

newtype QueueBuffer_ (size :: Nat) a = QueueBuffer_
  { q :: TBQueue a
  }

instance (KnownNat size) => Bufferable (QueueBuffer_ size) a where
  newBuf =
    QueueBuffer_ <$> newTBQueue (fromIntegral $ natVal (Proxy :: Proxy size))
  readBuf = readTBQueue . q
  writeBuf = writeTBQueue . q
  isEmptyBuf = isEmptyTBQueue . q
  bufHistory _ = return []

type QueueBuffer = QueueBuffer_ 64

data ValueBuffer a = ValueBuffer
  { val :: TMVar a
  , latest :: TVar (Maybe a)
  }

readJustTVar :: TVar (Maybe a) -> STM a
-- ^ Reimplementation of readTMVar for TVar (Maybe a).
{-# INLINE readJustTVar #-}
readJustTVar t = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a -> return a

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t a = tryTakeTMVar t >> putTMVar t a

instance Eq a => Bufferable ValueBuffer a where
  newBuf = ValueBuffer <$> newEmptyTMVar <*> newTVar Nothing
  readBuf = takeTMVar . val
  writeBuf t a = do
    l <- readTVar (latest t)
    when (Just a /= l) $ do
      writeTMVar (val t) a
      writeTVar (latest t) $ Just a
  isEmptyBuf = isEmptyTMVar . val
  bufHistory = fmap maybeToList . readTVar . latest

-- | Stream pusher type.
-- Also used internally to represent non-stream fanouts, like a tapStream.
data StreamPusher a = StreamPusher
  { pushSTM :: a -> STM Bool
  , closeSTM :: STM ()
  }

pushStream :: StreamPusher a -> a -> IO ()
pushStream t = void . atomically . pushSTM t

closeStream :: StreamPusher a -> IO ()
closeStream = atomically . closeSTM

instance Data.Semigroup.Semigroup (StreamPusher a) where
  i1 <> i2 =
    StreamPusher
      (\a -> (||) <$> pushSTM i1 a <*> pushSTM i2 a)
      (closeSTM i1 >> closeSTM i2)

instance Monoid (StreamPusher a) where
  mempty = StreamPusher (const $ return False) (return ())

-- | This instance is useful for creating new tagged address, similar to elm's
-- Signal.forwardTo. In fact elm's forwardTo is just 'flip contramap'
instance Contravariant StreamPusher where
  contramap f (StreamPusher push close) = StreamPusher (push . f) close

instance Divisible StreamPusher where
  conquer = mempty
  divide f i1 i2 =
    StreamPusher
      (\a ->
         case f a of
           (b, c) -> (||) <$> pushSTM i1 b <*> pushSTM i2 c)
      (closeSTM i1 >> closeSTM i2)

instance Decidable StreamPusher where
  lose f = StreamPusher (absurd . f) (return ())
  choose f i1 i2 =
    StreamPusher
      (\a ->
         case f a of
           Left b -> pushSTM i1 b
           Right c -> pushSTM i2 c)
      (closeSTM i1 >> closeSTM i2)

-- | Stream reader type.
-- TODO: generalize to monad transformer?
data Stream b a = Stream
  { buf :: b a -- exposed so you can implement specialized functions by buffer type
  , addDownstream :: StreamPusher a -> STM ()
  , addTap :: (a -> IO ()) -> IO () -- is IO because it may need to process history
  , propagator :: Async ()
  , isClosedStream :: STM Bool
  }

class Bufferable buf a =>
      Streamable buf a
  where
  newStream :: IO (StreamPusher a, Stream buf a)
  newStream = do
    buf <- atomically $ newBuf @buf
    downstreams <- TMap.newIO
    taps <- newTVarIO []
    fanCtr <- newTVarIO (0 :: Word)
    closedVar <- newTVarIO False
    let nextFanCnt = do
          c <- readTVar fanCtr
          writeTVar fanCtr $ c + 1
          return c
        isClosed = readTVar closedVar
        -- | wait until the stream has a consumer, then read a value
        -- or return Nothing immediately when the stream is closed and empty
        read =
          (do hasDownstreams <- not <$> TMap.null downstreams
              hasTaps <- not . null <$> readTVar taps
              check $ hasDownstreams || hasTaps
              Just <$> readBuf buf) <|>
          (do closed <- isClosed
              empty <- isEmptyBuf buf
              check $ closed && empty
              return Nothing)
        push a = do
          closed <- isClosed
          if closed
            then return False
            else do
              writeBuf buf a
              return True
        close = writeTVar closedVar True
        downstream pusher = do
          closed <- isClosed
          empty <- isEmptyBuf buf
          k <- nextFanCnt
          TMap.insert pusher k downstreams
          when empty $ bufHistory buf >>= mapM_ (pushSTM pusher)
          when (closed && empty) $ closeSTM pusher
        tap f = do
          empty <- atomically $ isEmptyBuf buf
          when empty $ atomically (bufHistory buf) >>= mapM_ f -- race condition?
          atomically $ modifyTVar' taps (f :)
        propagate = do
          m <- atomically read
          case m of
            Nothing ->
              atomically $ do
                TMap.values downstreams >>= mapM_ closeSTM
                -- allow garbage collection of fanouts
                TMap.reset downstreams
                writeTVar taps []
            Just a -> do
              TMap.parallelFilterM
                (\(_, pusher) -> atomically $ pushSTM pusher a)
                downstreams
              readTVarIO taps >>= Parallel.mapM ($ a)
              -- TODO: run downstreams and taps in parallel
              propagate
    propagator <- async propagate
    -- Automatically close when the sender goes out of scope
    -- or when (addDownstream and addTap go out of scope and no fanouts exist)
    rPush <- newTVarIO ()
    void $ mkWeakTVar rPush $ atomically close
    rFanout <- newTVarIO ()
    void $
      mkWeakTVar rFanout $
      atomically $ do
        TMap.null downstreams >>= check
        readTVar taps >>= check . null
        close
    let pusher = StreamPusher (\a -> readTVar rPush *> push a) close
        addDownstream ds = readTVar rFanout *> downstream ds
        addTap t = readTVarIO rFanout *> tap t
        stream = Stream buf addDownstream addTap propagator isClosed
    return (pusher, stream)
  downstreamM ::
       Bufferable buf b
    => ((b -> STM Bool) -> a -> IO ())
    -> Stream buf a
    -> IO (Stream buf b)
  {-# INLINE downstreamM #-}
  -- A downstream chained after an impure action is really just a special tap; even if the
  -- downstream closes, the action should still be evalutated to produce effects.
  -- That said, we still need to propagate the close event to the downstreamM.
  downstreamM f stream = do
    (StreamPusher {pushSTM, closeSTM}, stream') <- newStream
    addTap stream $ f pushSTM
    async $ waitStream stream >> atomically closeSTM
    return stream'
  filterMapMStream ::
       Bufferable buf b
    => (a -> IO (Maybe b))
    -> Stream buf a
    -> IO (Stream buf b)
  filterMapMStream f =
    downstreamM $ \push a ->
      void . runMaybeT $ MaybeT (f a) >>= lift . atomically . push
  filterMapStream ::
       Bufferable buf b => (a -> Maybe b) -> Stream buf a -> IO (Stream buf b)
  filterMapStream f Stream {addDownstream} = do
    (StreamPusher push close, stream) <- newStream
    atomically . addDownstream $
      StreamPusher
        (\a ->
           case f a of
             Nothing -> return True
             Just b -> push b)
        close
    return stream
  filterStream ::
       Bufferable buf a => (a -> Bool) -> Stream buf a -> IO (Stream buf a)
  filterStream f =
    filterMapStream $ \a ->
      if f a
        then Just a
        else Nothing
  mapMStream ::
       Bufferable buf b => (a -> IO b) -> Stream buf a -> IO (Stream buf b)
  mapMStream f = downstreamM $ \push a -> f a >>= void . atomically . push
  mapStream :: Bufferable buf b => (a -> b) -> Stream buf a -> IO (Stream buf b)
  mapStream f = filterMapStream $ Just . f
  tapStream_ :: (a -> IO ()) -> Stream buf a -> IO ()
  tapStream_ f Stream {addTap} = addTap f
  tapStream :: (a -> IO ()) -> Stream buf a -> IO (Stream buf a)
  tapStream f stream = do
    tapStream_ f stream
    return stream
  tapFoldStream ::
       (a -> accum -> IO accum) -> accum -> Stream buf a -> IO (STM accum)
  tapFoldStream f accum0 stream = do
    accumVar <- newTVarIO accum0
    let fold a = do
          accum <- readTVarIO accumVar
          accum' <- f a accum
          atomically $ writeTVar accumVar accum'
    tapStream_ fold stream
    return $ readTVar accumVar
  streamFromList :: [a] -> IO (Stream buf a)
  -- ^ Returns the stream immediately. A child thread fills it with the list contents.
  streamFromList as = do
    (pusher, stream) <- newStream
    void . async $ do
      mapM_ (pushStream pusher) as
      closeStream pusher
    return stream
  waitStream :: Stream buf a -> IO ()
  -- ^ Wait for this stream to close and empty, and for immediate taps to finish.
  -- Downstream taps may still be running.
  waitStream Stream {propagator} = wait propagator
  foldMStream :: (a -> accum -> IO accum) -> accum -> Stream buf a -> IO accum
  foldMStream f accum0 stream = do
    read <- tapFoldStream f accum0 stream
    waitStream stream
    atomically read
  foldStream :: (a -> accum -> accum) -> accum -> Stream buf a -> IO accum
  foldStream f = foldMStream (\a accum -> return $ f a accum)
  listFromStream :: Stream buf a -> IO [a]
  listFromStream as = foldStream (:) [] as >>- reverse
  consumeStream :: (a -> IO ()) -> Stream buf a -> IO ()
  consumeStream f stream = tapStream f stream >>= waitStream

instance Bufferable buf a => Streamable buf a

-- TODO: implement for all streams?
-- These fns block forever if the stream closes without ever sending any values...
-- what to do about that??
readLatestValueSTM :: Stream ValueBuffer a -> STM a
readLatestValueSTM = readJustTVar . latest . buf

readLatestValue :: Stream ValueBuffer a -> IO a
readLatestValue = atomically . readLatestValueSTM

readFinalValue :: Stream ValueBuffer a -> IO a
readFinalValue s =
  atomically $ do
    isClosedStream s >>= check
    readJustTVar $ latest $ buf s
