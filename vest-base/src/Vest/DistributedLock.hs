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
  }

defaultDistributedLock :: Connection -> Text' "LockId" -> DistributedLockConfig
defaultDistributedLock redis lockId =
  DistributedLockConfig {redis, lockId, renewInterval = sec 5}

acquire :: ByteString -> Integer -> Time Second -> Redis ()
-- ^ Polls every pollInterval until lock is acquired. Unfortunately there is no guaranteed way to
-- wake on-demand using event notifications (PubSub delivery is best-effort), and Redis lacks a
-- number of useful blocking calls.
acquire redisKey ttlSeconds pollInterval = do
  txResult <-
    multiExec $ do
      acquired <- setnx redisKey "0"
      expired <- expire redisKey ttlSeconds
       -- ^ setnx does not set any TTL on its own.
      return $ (&&) <$> acquired <*> expired
  case txResult of
    TxSuccess True -> return ()
    TxSuccess _ ->
      threadDelay pollInterval >> acquire redisKey ttlSeconds pollInterval
    _ -> liftIO $ throw RedisTransactionException

instance Resource DistributedLock where
  type ResourceConfig DistributedLock = DistributedLockConfig
  make :: DistributedLockConfig -> IO DistributedLock
  -- ^ Blocks until lock is acquired.
  make DistributedLockConfig {redis, lockId, renewInterval} = do
    let redisKey = encodeUtf8 $ untag lockId
        ttlSeconds = toNum @Second @Integer (2 *:* renewInterval)
    -- Default pollInterval is just the renewInterval.
    runRedis redis (acquire redisKey ttlSeconds renewInterval)
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
