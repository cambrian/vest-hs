-- | Asynchronous streams with buffering and fanout, a la Async.Pipe for OCaml
-- Future improvements:
-- - track the fanout tree and compress non-branching fanouts.
module Vest.Prelude.Stream
  ( Bufferable
  , QueueBuffer_
  , QueueBuffer
  , ValueBuffer
  , StreamPusher(..)
  , pushStream_
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
  , readLastValue
  ) where

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

-- | Backing storage for streams.
class Bufferable t a where
  newBuf :: STM (t a)
  readBuf :: t a -> STM (Maybe a) -- readBuffer should return Nothing as soon as the buffer closes
  writeBuf :: t a -> a -> STM ()
  closeBuf :: t a -> STM ()
  isClosedBuf :: t a -> STM Bool
  isEmptyBuf :: t a -> STM Bool

-- TODO: automatically close when out writer goes out of scope?
newtype QueueBuffer_ (size :: Nat) a = QueueBuffer_
  { q :: TBMQueue a
  }

instance (KnownNat size) => Bufferable (QueueBuffer_ size) a where
  newBuf =
    QueueBuffer_ <$> newTBMQueue (fromIntegral $ natVal (Proxy :: Proxy size))
  readBuf = readTBMQueue . q
  writeBuf = writeTBMQueue . q
  closeBuf = closeTBMQueue . q
  isClosedBuf = isClosedTBMQueue . q
  isEmptyBuf = isEmptyTBMQueue . q

type QueueBuffer = QueueBuffer_ 64

-- | Store closed separately from val because it's permitted to read the last value after a close
data ValueBuffer a = ValueBuffer
  { val :: TVar (Maybe a)
  , prev :: TVar (Maybe a)
  , closed :: TMVar ()
  }

readJustTVar :: TVar (Maybe a) -> STM a
-- reimplementation of readTMVar
{-# INLINE readJustTVar #-}
readJustTVar t = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a -> return a

instance Eq a => Bufferable ValueBuffer a where
  newBuf = ValueBuffer <$> newTVar Nothing <*> newTVar Nothing <*> newEmptyTMVar
  readBuf t =
    let read = do
          a <- readJustTVar (val t)
          p <- readTVar (prev t)
          check (Just a /= p)
          writeTVar (prev t) $ Just a
          return a
        readClosed = do
          closed <- isClosedBuf t
          check closed
          return Nothing
     in (Just <$> read) <|> readClosed
  writeBuf t = writeTVar (val t) . Just
  closeBuf t = void $ tryPutTMVar (closed t) ()
  isClosedBuf = fmap not . isEmptyTMVar . closed
  isEmptyBuf t = readTVar (val t) >>- isNothing

-- | Also used to represent non-stream fanouts, like a tapStream.
-- pushStream cannot be restricted to STM for this reason
-- although we could possibly introduce an isClosed :: STM Bool to improve the semantics
data StreamPusher a = StreamPusher
  { pushStream :: a -> IO Bool
  , closeStreamSTM :: STM ()
  }

pushStream_ :: StreamPusher a -> a -> IO ()
pushStream_ t = void . pushStream t

closeStream :: StreamPusher a -> IO ()
closeStream = atomically . closeStreamSTM

instance Data.Semigroup.Semigroup (StreamPusher a) where
  i1 <> i2 =
    StreamPusher
      (\a -> (||) <$> pushStream i1 a <*> pushStream i2 a)
      (closeStreamSTM i1 >> closeStreamSTM i2)

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
           (b, c) -> (||) <$> pushStream i1 b <*> pushStream i2 c)
      (closeStreamSTM i1 >> closeStreamSTM i2)

instance Decidable StreamPusher where
  lose f = StreamPusher (absurd . f) (return ())
  choose f i1 i2 =
    StreamPusher
      (\a ->
         case f a of
           Left b -> pushStream i1 b
           Right c -> pushStream i2 c)
      (closeStreamSTM i1 >> closeStreamSTM i2)

-- | TODO: generalize to monad transformer?
data Stream b a = Stream
  { buf :: b a
  , addFanout :: StreamPusher a -> IO ()
  , propagator :: Async ()
  }

class Bufferable buf a =>
      Streamable buf a
  where
  newStream :: IO (StreamPusher a, Stream buf a)
  {-^ Create a new Stream from the given buffer type.
      Using 'pushStream' on the 'StreamPusher'
          * does nothing and returns 'False' if the stream is closed, otherwise it:
          * blocks until the buffer is not full, adds the message, and returns 'True'.
      If the 'StreamPusher' is garbage collected the stream will automatically close.
      If a stream gets garbage collected without being consumed, it will also close.
  -}
  newStream = do
    buf <- atomically $ newBuf @buf
    fanouts <- TMap.newIO
    fanCtr <- newTVarIO (0 :: Word)
    let nextFanCnt = do
          c <- readTVar fanCtr
          writeTVar fanCtr (c + 1)
          return c
        push a = do
          closed <- isClosedBuf buf
          if closed
            then return False
            else do
              writeBuf buf a
              return True
        close = closeBuf buf
        fanout pusher = do
          closed <- isClosedBuf buf
          empty <- isEmptyBuf buf
          if closed && empty
            then closeStreamSTM pusher
            else do
              k <- nextFanCnt
              TMap.insert pusher k fanouts
        propagate = do
          m <- atomically (readBuf buf)
          case m of
            Nothing ->
              atomically $ do
                fs <- TMap.values fanouts
                mapM_ closeStreamSTM fs
                TMap.reset fanouts -- not sure if actually necessary
            Just a -> do
              atomically $ do
                numFanouts <- TMap.size fanouts
                check $ numFanouts > 0
              TMap.parallelFilterM (\(_, pusher) -> pushStream pusher a) fanouts
              propagate
    propagator <- async propagate
    -- Automatically close when the sender goes out of scope
    -- or when (addFanouts goes out of scope and no fanouts exist)
    rPush <- newTVarIO ()
    void $ mkWeakTVar rPush $ atomically close
    rFanout <- newTVarIO ()
    void $
      mkWeakTVar rFanout $
      atomically $ do
        TMap.null fanouts >>= check
        close
    let pusher =
          StreamPusher (\a -> atomically $ readTVar rPush *> push a) close
    let stream =
          Stream
            buf
            (\f -> atomically $ readTVar rFanout *> fanout f)
            propagator
    return (pusher, stream)
  fanoutStream ::
       Bufferable buf b
    => ((b -> IO Bool) -> a -> IO Bool)
    -> Stream buf a
    -> IO (Stream buf b)
  {-# INLINE fanoutStream #-}
  -- ^ Effectful pushers should always return true, otherwise they may be removed if their
  -- downstreams close.
  fanoutStream pusher Stream {addFanout} = do
    (StreamPusher push close, stream) <- newStream
    addFanout $ StreamPusher (pusher push) close
    return stream
  filterMapMStream ::
       Bufferable buf b
    => (a -> IO (Maybe b))
    -> Stream buf a
    -> IO (Stream buf b)
  filterMapMStream f =
    fanoutStream $ \pushStream a -> do
      runMaybeT $ MaybeT (f a) >>= lift . pushStream
      return True
  filterMapStream ::
       Bufferable buf b => (a -> Maybe b) -> Stream buf a -> IO (Stream buf b)
  filterMapStream f =
    fanoutStream $ \pushStream a ->
      case f a of
        Nothing -> return True
        Just b -> pushStream b
  filterStream ::
       Bufferable buf a => (a -> Bool) -> Stream buf a -> IO (Stream buf a)
  filterStream f =
    fanoutStream $ \pushStream a ->
      if f a
        then pushStream a
        else return True
  mapMStream ::
       Bufferable buf b => (a -> IO b) -> Stream buf a -> IO (Stream buf b)
  mapMStream f =
    fanoutStream $ \pushStream a -> f a >>= pushStream >> return True
  mapStream :: Bufferable buf b => (a -> b) -> Stream buf a -> IO (Stream buf b)
  mapStream f = fanoutStream $ \pushStream -> pushStream . f
  tapStream_ :: (a -> IO ()) -> Stream buf a -> IO ()
  tapStream_ f Stream {addFanout} =
    addFanout $ StreamPusher (\a -> f a >> return True) (return ())
  tapStream :: (a -> IO ()) -> Stream buf a -> IO (Stream buf a)
  tapStream f stream = do
    tapStream_ f stream
    return stream
  tapFoldStream ::
       (accum -> a -> IO accum) -> accum -> Stream buf a -> IO (STM accum)
  tapFoldStream f accum0 stream = do
    accumVar <- newTVarIO accum0
    let fold a = do
          accum <- readTVarIO accumVar
          accum' <- f accum a
          atomically $ writeTVar accumVar accum'
    tapStream_ fold stream
    return $ readTVar accumVar
  streamFromList :: [a] -> IO (Stream buf a)
  streamFromList as = do
    (pusher, stream) <- newStream
    mapM_ (pushStream pusher) as
    closeStream pusher
    return stream
  waitStream :: Stream buf a -> IO ()
  waitStream Stream {propagator} = wait propagator
  foldMStream :: (accum -> a -> IO accum) -> accum -> Stream buf a -> IO accum
  foldMStream f accum0 stream = do
    read <- tapFoldStream f accum0 stream
    waitStream stream
    atomically read
  foldStream :: (accum -> a -> accum) -> accum -> Stream buf a -> IO accum
  foldStream f = foldMStream (\accum a -> return $ f accum a)
  listFromStream :: Stream buf a -> IO [a]
  listFromStream as = foldStream (flip (:)) [] as >>- reverse
  consumeStream :: (a -> IO ()) -> Stream buf a -> IO ()
  consumeStream f stream = tapStream f stream >>= waitStream

instance Bufferable buf a => Streamable buf a

-- TODO: implement for all streams?
readLatestValueSTM :: Stream ValueBuffer a -> STM a
readLatestValueSTM = readJustTVar . val . buf

readLatestValue :: Stream ValueBuffer a -> IO a
readLatestValue = atomically . readLatestValueSTM

readLastValue :: Bufferable ValueBuffer a => Stream ValueBuffer a -> IO a
readLastValue t = waitStream t >> readLatestValue t
