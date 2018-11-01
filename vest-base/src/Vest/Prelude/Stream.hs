-- | Asynchronous streams with buffering and fanout, a la Async.Pipe for OCaml
--
-- Future improvements:
-- - merging. Streams will most likely have to maintain a list of writers, and close only after
--   all of their writers do.
-- - track and compress the fanout tree.
--
-- OVERVIEW:
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
-- Downstreams:
-- A downstream is any consumer of a stream's values; a downstream need not be a StreamWriter
-- itself. A stream may fanout to any number of downstreams, which all run in parallel.
module Vest.Prelude.Stream
  ( BufferWriter(..)
  , BufferReader(..)
  , Bufferable
  , QueueBuffer_
  , QueueBuffer
  , ValueBuffer
  , StreamWriter(..)
  , writeStream
  , Stream(..)
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
  , streamFirst
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

data BufferWriter a = BufferWriter
  { writeBuf :: a -> STM Bool
  , closeBuf :: STM ()
  }

data BufferReader a = BufferReader
  { readBuf :: STM a
  , isClosedBuf :: STM Bool
  , isEmptyBuf :: STM Bool
  , bufHistory :: STM [a] -- ^ Somewhat hacky. Anything returned by this function is pushed to new fanouts before they receive future updates. Used by ValueBuffers
  }

class Bufferable t a where
  newBuf :: STM (BufferWriter a, BufferReader a)

data QueueBuffer_ (size :: Nat)

instance (KnownNat size) => Bufferable (QueueBuffer_ size) a where
  newBuf = do
    q <- newTBMQueue (fromIntegral $ natVal (Proxy :: Proxy size))
    let isClosedBuf = isClosedTBMQueue q
        writeBuf a = do
          closed <- isClosedBuf
          if closed
            then return False
            else do
              writeTBMQueue q a
              return True
        readBuf = justSTM $ readTBMQueue q
        writer = BufferWriter writeBuf (closeTBMQueue q)
        reader =
          BufferReader readBuf isClosedBuf (isEmptyTBMQueue q) (return [])
    return (writer, reader)

type QueueBuffer = QueueBuffer_ 64

data ValueBuffer a

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t a = tryTakeTMVar t >> putTMVar t a

instance Eq a => Bufferable ValueBuffer a where
  newBuf = do
    val <- newEmptyTMVar
    latest <- newTVar Nothing
    closed <- newTVar False
    let isClosedBuf = readTVar closed
        writeBuf a = do
          closed <- isClosedBuf
          if closed
            then return False
            else do
              l <- readTVar latest
              when (Just a /= l) $ do
                writeTMVar val a
                writeTVar latest $ Just a
              return True
        writer = BufferWriter writeBuf (writeTVar closed True)
        reader =
          BufferReader
            (takeTMVar val)
            isClosedBuf
            (isEmptyTMVar val)
            (maybeToList <$> readTVar latest)
    return (writer, reader)

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
         t1 <- async $ writeStream' i1 a
         t2 <- async $ writeStream' i2 a
         (r1, r2) <- waitBoth t1 t2
         return $ r1 || r2)
      (closeStream i1 >> closeStream i2)

instance Monoid (StreamWriter a) where
  mempty = StreamWriter (const $ return False) (return ())

instance Contravariant StreamWriter where
  contramap f (StreamWriter write close) = StreamWriter (write . f) close

contraM :: ((b -> IO Bool) -> a -> IO Bool) -> StreamWriter b -> StreamWriter a
{-# INLINE contraM #-}
contraM f (StreamWriter write close) = StreamWriter (f write) close

instance Divisible StreamWriter where
  conquer = mempty
  divide f i1 i2 =
    StreamWriter
      (\a -> do
         let (b, c) = f a
         t1 <- async $ writeStream' i1 b
         t2 <- async $ writeStream' i2 c
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

makeStreamWriter :: BufferWriter a -> IO (StreamWriter a)
makeStreamWriter BufferWriter {writeBuf, closeBuf} =
  return $ StreamWriter (atomically . writeBuf) (atomically closeBuf)

-- TODO: generalize to monad transformer?
data Stream buf a = Stream
  { addDownstream :: StreamWriter a -> IO () -- is IO because it has to process message history
  , isClosedStream :: STM Bool
  , isFinishedStream :: STM Bool -- closed, empty, and all non-downstream effects are finished
  , streamHistory :: STM [a]
  }

makeStreamReader :: BufferReader a -> IO (Stream buf a)
makeStreamReader BufferReader {readBuf, isClosedBuf, isEmptyBuf, bufHistory} = do
  downstreams <- TMap.newIO
  downstreamCtr <- newTVarIO (0 :: Word)
  propagateLock <- Lock.new
  -- We lock the propagation during addDownstream because of processing message history.
  -- Manual locking is required (or async gymnastics, which was uglier), because processing message
  -- history is impure.
  -- TODO: refactor to stateTVar when stm-2.5 is available
  let nextDownstreamCnt = do
        c <- readTVar downstreamCtr
        writeTVar downstreamCtr $ c + 1
        return c
      -- | wait until the stream has a consumer, then read a value
      -- or return Nothing immediately when the stream is closed and empty
      read =
        (do TMap.null downstreams >>= check . not
            Just <$> readBuf) <|>
        (do closed <- isClosedBuf
            empty <- isEmptyBuf
            check $ closed && empty
            return Nothing)
      addDownstream writer = do
        Lock.acquire propagateLock
        c <- atomically nextDownstreamCnt
        atomically bufHistory >>= mapM_ (writeStream' writer)
        atomically $ TMap.insert writer c downstreams
        closed <- atomically isClosedBuf
        empty <- atomically isEmptyBuf
        when (closed && empty) $ closeStream writer
        Lock.release propagateLock
      propagate = do
        m <- atomically read
        Lock.acquire propagateLock
        case m of
          Nothing -> do
            TMap.parallelMapValuesM_ closeStream downstreams
            atomically $ TMap.reset downstreams -- Just for good measure
            Lock.release propagateLock
          Just a -> do
            TMap.parallelMapValuesM_ (`writeStream'` a) downstreams
            Lock.release propagateLock
            propagate
  propagator <- async propagate
  let isFinished = isJust <$> pollSTM propagator
  return $ Stream addDownstream isClosedBuf isFinished bufHistory

class Bufferable buf a =>
      Streamable buf a
  where
  newStream :: IO (StreamWriter a, Stream buf a)
  newStream = do
    (bw, br) <- atomically $ newBuf @buf
    writer <- makeStreamWriter bw
    reader <- makeStreamReader br
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
  takeWhileMStream f Stream {addDownstream} = do
    (writer, reader) <- newStream
    addDownstream $
      contraM
        (\write a -> do
           take <- f a
           if take
             then write a
             else return False)
        writer
    return reader
  takeStream :: Natural -> Stream buf a -> IO (Stream buf a)
  takeStream n stream = do
    ctr <- newTVarIO 0
    takeWhileMStream
      (const $ atomically $ do
         c <- readTVar ctr
         if c < n
           then do
             writeTVar ctr $ c + 1
             return True
           else return False)
      stream
  streamFirst :: Stream buf a -> IO a
  streamFirst stream =
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

-- TODO: implement for all streams?
readLatestValueSTM :: Stream ValueBuffer a -> STM (Maybe a)
readLatestValueSTM s = last <$> streamHistory s

readLatestValue :: Stream ValueBuffer a -> IO (Maybe a)
readLatestValue = atomically . readLatestValueSTM

readFinalValue :: Stream ValueBuffer a -> IO (Maybe a)
readFinalValue s =
  atomically $ do
    isClosedStream s >>= check
    last <$> streamHistory s
