module Vest.DistributedLock
  ( defaultDistributedLock
  , DistributedLockConfig(..)
  , DistributedLock
  , withDistributedLock
  ) where

import Database.Redis
import Vest.Prelude hiding (get)
import qualified Vest.Prelude
import Vest.Redis

-- | Works via the setnx command, which succeeds only if the desired key doesn't already exist.
-- Note that this algorithm will not work with several Redis masters (at which point we should
-- consider implementing the Redlock algorithm).
data DistributedLock = DistributedLock
  { redis :: RedisConnection
  , lockId :: Text' "LockId"
  , cancelRenewer :: IO' "CancelInterval" ()
  }

data DistributedLockConfig = DistributedLockConfig
  { redis :: RedisConnection
  , lockId :: Text' "LockId"
  , renewInterval :: Duration
  , pollInterval :: Duration
  }

defaultDistributedLock ::
     RedisConnection -> Text' "LockId" -> DistributedLockConfig
defaultDistributedLock redis lockId =
  DistributedLockConfig
    {redis, lockId, renewInterval = sec 5, pollInterval = sec 5}

-- | Wait for an event on the given key but un-block eventually in case of a network glitch.
waitThenPoll :: PubSubController -> ByteString -> Duration -> IO ()
waitThenPoll pubSub redisKey pollInterval = do
  done <- newEmptyTMVarIO
  let channel = "__keyspace@0__:" <> redisKey
  void . async $ threadDelay pollInterval >> atomically (putTMVar done ())
  -- ^ Auto-unblock the waiter after pollInterval is over.
  removeKeySubscription <-
    addChannels pubSub [(channel, const $ atomically $ putTMVar done ())] []
  atomically $ takeTMVar done
  removeKeySubscription

acquire :: PubSubController -> ByteString -> Integer -> Duration -> Redis ()
acquire pubSub redisKey ttlSeconds pollInterval = do
  txResult <-
    multiExec $ do
      acquired <- setnx redisKey "0"
      expired <- expire redisKey ttlSeconds
       -- ^ setnx does not set any TTL on its own.
      return $ (&&) <$> acquired <*> expired
  case txResult of
    TxSuccess True -> return ()
    TxSuccess _ ->
      liftIO (waitThenPoll pubSub redisKey pollInterval) >>
      acquire pubSub redisKey ttlSeconds pollInterval
      -- ^ Block on key then retry acquire (see waitThenPoll).
    _ -> liftIO $ throw RedisTransactionException

instance Resource DistributedLock where
  type ResourceConfig DistributedLock = DistributedLockConfig
  make :: DistributedLockConfig -> IO DistributedLock
  -- ^ Blocks until lock is acquired.
  make DistributedLockConfig {redis, lockId, renewInterval, pollInterval} = do
    let redisKey = encodeUtf8 $ untag lockId
        ttlSeconds = round $ toRational $ 2 *^ renewInterval
    pubSub <- newPubSubController [] []
    async $ forever $ pubSubForever redis pubSub (return ())
    -- ^ Setup a manager thread for pubSub (see hedis docs).
    runRedis redis (acquire pubSub redisKey ttlSeconds pollInterval)
    (_, cancelRenewer) <-
      intervalRenewable
        renewInterval
        (void . runRedis redis $ expire redisKey ttlSeconds)
    return DistributedLock {redis, lockId, cancelRenewer}
  cleanup :: DistributedLock -> IO ()
  cleanup DistributedLock {redis, lockId, cancelRenewer} = do
    let redisKey = encodeUtf8 . untag $ lockId
    untag cancelRenewer
    void . runRedis redis $ del [redisKey]

withDistributedLock ::
     Has RedisConnection t => t -> Text' "LockId" -> IO a -> IO a
withDistributedLock t lockId =
  with @DistributedLock (defaultDistributedLock (Vest.Prelude.get t) lockId) .
  const
