-- | Asynchronous streams with buffering and fanout, a la Async.Pipe for OCaml
--
-- OVERVIEW:
--
-- Buffering:
-- Each stream is backed by a buffer. Internally a child thread (propagator) attempts to unbuffer
-- items and propagate them downstream. The propagator waits for downstream writes to finish (in
-- parallel) before unbuffering the next item. This means that items may pile up inside
-- the buffer if the stream is written to faster its downstreams can handle. Writes block until
-- the buffer accepts them.
--
-- Fanout:
-- A stream may have any number of downstream consumers, which are written to in parallel.
-- Propagation proceeds at the pace of the slowest consumer, so a single slow consumer may cause
-- buffering upstream. Consider using a ValueBuffer if not every value can/needs to be processed.
--
-- TODO:
-- - Implement mergeStreams
-- - Make pure filterMaps not buffered. This would be easiest to do by providing a functor instance
--   for Stream buf, however doing so would also require a functor instance for buf, which currently
--   cannot be well-defined. A slight refactor could enable a functor instance, but adding the
--   filter part of filtermap would still be challenging.
module Vest.Prelude.Stream
  ( Bufferable
  , QueueBuffer_
  , QueueBuffer
  , ValueBuffer
  , StreamWriter(..)
  , writeStream
  , Stream
  , makeStreamWriter
  , makeStreamReader
  , newStream
  , filterMapMStream
  , filterMapStream
  , filterStream
  , mapMStream
  , mapStream
  , tapStream
  , tapStream_
  , tapFoldStream
  , takeWhileMStream
  , takeStream
  , streamNext
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

import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.STM.TBMQueue
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Functor.Contravariant.Divisible
  ( Decidable(choose, lose)
  , Divisible(conquer, divide)
  )
import Data.Monoid (Monoid(mempty))
import Data.Semigroup
import qualified TMap
import Vest.Prelude.Core

-- Eddie: I wanted to separate this into BufWriters and BufReaders but GHC didn't like it. Maybe
-- try again at some point.
class Bufferable t a where
  newBuf :: STM (t a)
  writeBuf :: t a -> a -> STM Bool
  closeBuf :: t a -> STM ()
  readBuf :: t a -> STM a
  isClosedBuf :: t a -> STM Bool
  isEmptyBuf :: t a -> STM Bool
  bufHistory :: t a -> STM [a]
  isFinishedBuf :: t a -> STM Bool
  isFinishedBuf t = (&&) <$> isClosedBuf t <*> isEmptyBuf t

newtype QueueBuffer_ (size :: Nat) a = QueueBuffer_
  { q :: TBMQueue a
  }

instance (KnownNat size) => Bufferable (QueueBuffer_ size) a where
  newBuf =
    QueueBuffer_ <$> newTBMQueue (fromIntegral (natVal (Proxy :: Proxy size)))
  isClosedBuf = isClosedTBMQueue . q
  writeBuf t a = do
    closed <- isClosedBuf t
    if closed
      then return False
      else do
        writeTBMQueue (q t) a
        return True
  readBuf = justSTM . readTBMQueue . q
  closeBuf = closeTBMQueue . q
  isEmptyBuf = isEmptyTBMQueue . q
  bufHistory _ = return []

type QueueBuffer = QueueBuffer_ 64

data ValueBuffer a = ValueBuffer
  { val :: TMVar a
  , latest :: TVar (Maybe a)
  , closed :: TVar Bool
  }

instance Eq a => Bufferable ValueBuffer a where
  newBuf = ValueBuffer <$> newEmptyTMVar <*> newTVar Nothing <*> newTVar False
  isClosedBuf = readTVar . closed
  writeBuf t a = do
    closed <- isClosedBuf t
    if closed
      then return False
      else do
        late <- readTVar $ latest t
        when (Just a /= late) $ do
          writeTMVar (val t) a
          writeTVar (latest t) $ Just a
        return True
  readBuf = takeTMVar . val
  closeBuf t = writeTVar (closed t) True
  isEmptyBuf = isEmptyTMVar . val
  bufHistory t = do
    empty <- isEmptyBuf t
    if empty
      then maybeToList <$> readTVar (latest t)
      else return []

-- | Also used internally to represent effects to be run per-item
data StreamWriter a = StreamWriter
  { writeStream' :: a -> IO Bool
  , closeStream :: IO ()
  }

writeStream :: StreamWriter a -> a -> IO ()
writeStream w a = void $ writeStream' w a

instance Data.Semigroup.Semigroup (StreamWriter a) where
  i1 <> i2 =
    StreamWriter
      (\a -> do
         t1 <- asyncDetached $ writeStream' i1 a
         t2 <- asyncDetached $ writeStream' i2 a
         (r1, r2) <- waitBoth t1 t2
         return $ r1 || r2)
      (closeStream i1 >> closeStream i2)

instance Monoid (StreamWriter a) where
  mempty = StreamWriter (const $ return False) (return ())

instance Contravariant StreamWriter where
  contramap f (StreamWriter write close) = StreamWriter (write . f) close

contraM :: ((b -> IO Bool) -> a -> IO Bool) -> StreamWriter b -> StreamWriter a
-- ^ Generalized monadic contramap
{-# INLINE contraM #-}
contraM f (StreamWriter write close) = StreamWriter (f write) close

instance Divisible StreamWriter where
  conquer = mempty
  divide f i1 i2 =
    StreamWriter
      (\a -> do
         let (b, c) = f a
         t1 <- asyncDetached $ writeStream' i1 b
         t2 <- asyncDetached $ writeStream' i2 c
         (r1, r2) <- waitBoth t1 t2
         return $ r1 || r2)
      (closeStream i1 >> closeStream i2)

-- TODO: has a bug where only one stream closing can cause the combined streamwriter to close
instance Decidable StreamWriter where
  lose f = StreamWriter (absurd . f) (return ())
  choose f i1 i2 =
    StreamWriter
      (\a ->
         case f a of
           Left b -> writeStream' i1 b
           Right c -> writeStream' i2 c)
      (closeStream i1 >> closeStream i2)

makeStreamWriter :: Bufferable buf a => buf a -> IO (StreamWriter a)
makeStreamWriter buf =
  return $ StreamWriter (atomically . writeBuf buf) (atomically $ closeBuf buf)

-- TODO: generalize to monad transformer around monadIO?
data Stream buf a = Stream
  { addDownstream :: StreamWriter a -> IO () -- is IO because it has to process message history
  , isClosedStream :: STM Bool
  , isFinishedStream :: STM Bool -- closed, empty, and all non-downstream effects are finished
  , buf :: buf a -- Hacky way to allow buffer-specific functions.
  }

makeStreamReader :: Bufferable buf a => buf a -> IO (Stream buf a)
makeStreamReader buf = do
  thisThread <- myThreadId
  downstreams <- TMap.newIO
  downstreamCtr <- newTVarIO (0 :: Word)
  propagateLock <- Lock.new
  -- We lock the propagation during addDownstream because of processing message history.
  -- Manual locking is required (or async gymnastics, which is worse), because processing
  -- message history cannot generally be run in STM.
  -- TODO: refactor to stateTVar when stm-2.5 is available
  let nextDownstreamCnt = do
        c <- readTVar downstreamCtr
        writeTVar downstreamCtr $ c + 1
        return c
      -- | wait until the stream has a consumer, then read a value
      -- or return Nothing immediately when the buffer finishes
      read =
        (do TMap.null downstreams >>= check . not
            Just <$> readBuf buf) <|>
        (do isFinishedBuf buf >>= check
            return Nothing)
      addDownstream writer =
        Lock.with propagateLock $ do
          c <- atomically nextDownstreamCnt
          atomically (bufHistory buf) >>= mapM_ (writeStream' writer)
          finished <- atomically $ isFinishedBuf buf
          if finished
            then closeStream writer
            else atomically $ TMap.insert writer c downstreams
      propagate = do
        a <- MaybeT $ atomically read
        lift $ Lock.with propagateLock $
          -- Uncaught exceptions in impure streaming actions should cause the process to crash. Note
          -- that parallelFilterValuesM runs threads internally, so we cannot simply wrap propagate
          -- in an async block.
          TMap.parallelFilterValuesM (`writeStream'` a) downstreams `catchAny`
          throwTo thisThread
        propagate
  propagator <-
    asyncDetached $ do
      void $ runMaybeT propagate
      atomically (TMap.values downstreams) >>= mapM_ closeStream
      atomically $ TMap.reset downstreams -- Just for good measure
  let isFinishedStream = isJust <$> pollSTM propagator
  return $ Stream addDownstream (isClosedBuf buf) isFinishedStream buf

class Bufferable buf a =>
      Streamable buf a
  where
  newStream :: IO (StreamWriter a, Stream buf a)
  newStream = do
    buf <- atomically newBuf
    writer <- makeStreamWriter buf
    reader <- makeStreamReader buf
    return (writer, reader)
  downstream ::
       Bufferable buf b
    => ((b -> IO Bool) -> a -> IO Bool)
    -> Stream buf a
    -> IO (Stream buf b)
  {-# INLINE downstream #-}
  downstream f Stream {addDownstream} = do
    (writer, reader) <- newStream
    addDownstream (contraM f writer)
    return reader
  filterMapMStream ::
       Bufferable buf b
    => (a -> IO (Maybe b))
    -> Stream buf a
    -> IO (Stream buf b)
  filterMapMStream f =
    downstream $ \write a ->
      runMaybeT (MaybeT (f a) >>= lift . write) >>- fromMaybe True
  filterMapStream ::
       Bufferable buf b => (a -> Maybe b) -> Stream buf a -> IO (Stream buf b)
  filterMapStream = filterMapMStream . (return <$>)
  filterStream ::
       Bufferable buf a => (a -> Bool) -> Stream buf a -> IO (Stream buf a)
  filterStream f =
    filterMapStream $ \a ->
      if f a
        then Just a
        else Nothing
  mapMStream ::
       Bufferable buf b => (a -> IO b) -> Stream buf a -> IO (Stream buf b)
  mapMStream f = downstream $ \write a -> f a >>= write
  mapStream :: Bufferable buf b => (a -> b) -> Stream buf a -> IO (Stream buf b)
  mapStream f = filterMapStream $ Just . f
  tapStream_ :: (a -> IO ()) -> Stream buf a -> IO ()
  tapStream_ f Stream {addDownstream} =
    addDownstream $ contraM (\_ a -> f a >> return True) mempty
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
  takeWhileMStream :: (a -> IO Bool) -> Stream buf a -> IO (Stream buf a)
  -- ^ Removes itself from the parent's downstreams after f returns False.
  takeWhileMStream f Stream {addDownstream} = do
    (writer, reader) <- newStream
    addDownstream $
      contraM
        (\write a -> do
           take <- f a
           if take
             then write a
             else do
               closeStream writer
               return False)
        writer
    return reader
  takeStream :: Natural -> Stream buf a -> IO (Stream buf a)
  -- ^ Removes itself from the parent's downstreams after n items have been taken.
  -- NB: includes streamHistory in the count
  -- Currently buggy when n=0
  takeStream n Stream {addDownstream} = do
    ctr <- newTVarIO 0
    (writer, reader) <- newStream
    addDownstream $
      contraM
        (\write a -> do
           c <- readTVarIO ctr
           when (c < n) $ void $ write a
           if c < n - 1
             then do
               atomically $ writeTVar ctr $ c + 1
               return True
             else do
               closeStream writer
               return False)
        writer
    return reader
  streamNext :: Stream buf a -> IO a
  -- ^ TODO: performant specialization?
  streamNext stream =
    takeStream 1 stream >>= listFromStream >>- fromMaybe (panic "bug") . last
  streamFromList :: [a] -> IO (Stream buf a)
  -- ^ Returns the stream immediately. A child thread fills it with the list contents.
  streamFromList as = do
    (writer, reader) <- newStream
    void . async $ do
      mapM_ (writeStream' writer) as
      closeStream writer
    return reader
  waitStream :: Stream buf a -> IO ()
  -- ^ Wait for this stream to closeStream and empty, and for immediate taps to finish.
  -- Downstream taps may still be running.
  waitStream Stream {isFinishedStream} = atomically $ isFinishedStream >>= check
  foldMStream :: (a -> accum -> IO accum) -> accum -> Stream buf a -> IO accum
  foldMStream f accum0 stream = do
    read <- tapFoldStream f accum0 stream
    waitStream stream
    atomically read
  foldStream :: (a -> accum -> accum) -> accum -> Stream buf a -> IO accum
  foldStream f = foldMStream (\a accum -> return $ f a accum)
  listFromStream :: Stream buf a -> IO [a]
  listFromStream as = reverse <$> foldStream (:) [] as
  consumeStream :: (a -> IO ()) -> Stream buf a -> IO ()
  consumeStream f stream = tapStream f stream >>= waitStream

instance Bufferable buf a => Streamable buf a

readLatestValueSTM :: Stream ValueBuffer a -> STM (Maybe a)
readLatestValueSTM = readTVar . latest . buf

readLatestValue :: Stream ValueBuffer a -> IO (Maybe a)
readLatestValue = atomically . readLatestValueSTM

readFinalValue :: Stream ValueBuffer a -> IO (Maybe a)
readFinalValue s =
  atomically $ do
    isClosedStream s >>= check
    readLatestValueSTM s
