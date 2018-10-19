module Vest.DistributedLock
  ( defaultDistributedLock
  , DistributedLockConfig(..)
  , DistributedLock
  ) where

import Database.Redis
import Vest.Prelude hiding (get)
import Vest.Redis

-- | Works via the setnx command, which succeeds only if the desired key doesn't already exist.
-- Note that this algorithm will not work with several Redis masters (at which point we should
-- consider implementing the Redlock algorithm).
data DistributedLock = DistributedLock
  { redis :: Connection
  , lockId :: Text' "LockId"
  , cancelRenewer :: IO' "CancelInterval" ()
  }

data DistributedLockConfig = DistributedLockConfig
  { redis :: Connection
  , lockId :: Text' "LockId"
  , renewInterval :: Time Second
  , pollInterval :: Time Millisecond
  }

defaultDistributedLock :: Connection -> Text' "LockId" -> DistributedLockConfig
defaultDistributedLock redis lockId =
  DistributedLockConfig
    {redis, lockId, renewInterval = sec 5, pollInterval = ms 5000}

-- Sadly, pubSub blocks in the Redis monad, so this function has to be IO.
waitForEvent :: Connection -> ByteString -> Time Millisecond -> IO ()
waitForEvent conn redisKey pollInterval = do
  eventOccurred <- liftIO newEmptyTMVarIO
  let channel = "__keyspace@0__:" <> redisKey
  async $ threadDelay pollInterval >> atomically (putTMVar eventOccurred ())
  -- ^ If no event arrives, auto-unblock after period pollInterval.
  async $
    runRedis conn $
    pubSub (subscribe [channel]) $
    const $ do
      liftIO $ atomically $ putTMVar eventOccurred ()
      return $ unsubscribe [channel]
  liftIO $ atomically $ takeTMVar eventOccurred

acquire :: Connection -> ByteString -> Integer -> Time Millisecond -> IO ()
-- ^ Polls every pollInterval until lock is acquired. Unfortunately there is no guaranteed way to
-- wake on-demand using event notifications (PubSub delivery is best-effort), and Redis lacks a
-- number of useful blocking calls.
acquire redis redisKey ttlSeconds pollInterval = do
  txResult <-
    runRedis redis $
    multiExec $ do
      acquired <- setnx redisKey "0"
      expired <- expire redisKey ttlSeconds
       -- ^ setnx does not set any TTL on its own.
      return $ (&&) <$> acquired <*> expired
  case txResult of
    TxSuccess True -> return ()
    TxSuccess _ ->
      waitForEvent redis redisKey pollInterval >>
      acquire redis redisKey ttlSeconds pollInterval
    _ -> throw RedisTransactionException

instance Resource DistributedLock where
  type ResourceConfig DistributedLock = DistributedLockConfig
  make :: DistributedLockConfig -> IO DistributedLock
  -- ^ Blocks until lock is acquired.
  make DistributedLockConfig {redis, lockId, renewInterval, pollInterval} = do
    let redisKey = encodeUtf8 $ untag lockId
        ttlSeconds = toNum @Second @Integer (2 *:* renewInterval)
    acquire redis redisKey ttlSeconds pollInterval
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
