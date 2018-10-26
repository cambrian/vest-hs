module PipeDraft
  ( Pipe(..)
  , newPipe
  , mapPipe
  , consumePipe
  , unbounded
  , bounded
  , newest
  , latest
  ) where

import Pipes.Concurrent
import Vest

newtype Pipe a = Pipe
  { consumers :: TVar [(a -> IO (), IO ())]
  }

fanout :: Input a -> TVar [(a -> IO (), IO ())] -> IO ()
fanout readEnd consumersVar = do
  (itemMaybe, consumers) <-
    atomically $ do
      consumers <- readTVar consumersVar
      -- Ensure that one consumer is bound.
      check (not $ null consumers)
      itemMaybe <- recv readEnd
      return (itemMaybe, consumers)
  case itemMaybe of
    Just item -> mapM_ (($ item) . fst) consumers >> fanout readEnd consumersVar
    Nothing -> mapM_ snd consumers

-- | Creates a new pipe consisting of (push, close, pipe).
-- Values pushed to the pipe will remain in the pipe until at least one consumer is bound to the
-- pipe. When multiple consumers are bound, pushed values will fan out to all consumers.
newPipe :: Buffer a -> IO (a -> IO (), IO (), Pipe a)
newPipe bufferType = do
  (writeEnd, readEnd, close) <- spawn' bufferType
  consumers <- newTVarIO []
  async $ fanout readEnd consumers
  return (void . atomically . send writeEnd, atomically close, Pipe consumers)

-- | Consumes a pipe indirectly by building a new pipe from the passed transformation.
mapPipe :: (a -> IO b) -> Pipe a -> IO (Pipe b)
mapPipe f (Pipe consumers) = do
  (writeEnd, readEnd, close) <- spawn' unbounded
  atomically $
    modifyTVar'
      consumers
      ((f >=> (void . atomically . send writeEnd), atomically close) :)
  consumers <- newTVarIO []
  async $ fanout readEnd consumers
  return $ Pipe consumers

-- | Consumes a pipe directly with the passed action.
consumePipe :: (a -> IO ()) -> Pipe a -> IO ()
consumePipe f (Pipe consumers) = do
  (writeEnd, readEnd, close) <- spawn' unbounded
  atomically $
    modifyTVar'
      consumers
      ((void . atomically . send writeEnd, atomically close) :)
  let recvLoop = do
        itemMaybe <- atomically $ recv readEnd
        case itemMaybe of
          Just item -> f item >> recvLoop
          Nothing -> return ()
  recvLoop >> atomically close
