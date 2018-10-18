module Vest.DistLock
  ( defaultLock
  , T(..)
  ) where

import Database.Redis
import Vest.Prelude.Core hiding (get)
import Vest.Prelude.Resource
import Vest.Prelude.Time
import Vest.Prelude.UUID
import qualified Vest.Redis as Redis

data T = T
  { conn :: Redis.T
  , key :: Text' "LockKey"
  , value :: Text' "LockValue"
  , renewThread :: Async ()
  }

data Config = Config
  { conn :: Redis.T
  , key :: Text' "LockKey"
  , ttl :: Time Second
  , renewInterval :: Time Second
  }

data CacheStateException =
  CacheStateException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

defaultLock :: Redis.T -> Text' "LockKey" -> ResourceConfig T
defaultLock conn key = Config {conn, key, ttl = sec 10, renewInterval = sec 5}

waitForKey :: ByteString -> Redis ()
waitForKey key = do
  waitForResult <- liftIO newEmptyMVar
  let channel = "__keyspace@0__:" <> key
  pubSub (subscribe [channel]) $
    const $ do
      liftIO (putMVar waitForResult ())
      return $ unsubscribe [channel]
  liftIO $ takeMVar waitForResult

acquire :: ByteString -> ByteString -> Integer -> Redis ()
acquire key value ttl = do
  txResult <-
    multiExec $ do
      acquired <- setnx key value
      expired <- expire key ttl
       -- ^ setnx does not set any TTL on its own.
      return $ (&&) <$> acquired <*> expired
  case txResult of
    TxSuccess True -> return ()
    TxSuccess _ -> waitForKey key >> acquire key value ttl
    _ -> liftIO $ throw Redis.TransactionException

renew :: ByteString -> ByteString -> Integer -> Time Second -> Redis ()
renew key value ttl renewInterval = do
  liftIO $ threadDelay renewInterval
  txResult <-
    multiExec $ do
      keyValue <- get key
      renewed <- expire key ttl
      return $ (,) <$> keyValue <*> renewed
  -- On renew, ensure that we still own the lock.
  case txResult of
    TxSuccess (val, True)
      | val == Just value -> renew key value ttl renewInterval
    TxSuccess _ -> liftIO $ throw CacheStateException
    _ -> liftIO $ throw Redis.TransactionException

delete :: ByteString -> ByteString -> Redis ()
delete key value = do
  txResult <-
    multiExec $ do
      keyValue <- get key
      numDeleted <- del [key]
      return $ (,) <$> keyValue <*> numDeleted
      -- On release, ensure that we still own the lock.
  case txResult of
    TxSuccess (val, 1)
      | val == Just value -> return ()
    TxSuccess _ -> liftIO $ throw CacheStateException
    _ -> liftIO $ throw Redis.TransactionException

-- | Uses a simple locking algorithm: Set a unique value on the lock key iff it is not already set.
-- Note that this algorithm will not work with several Redis masters (at which point we should
-- consider implementing the Redlock algorithm).
instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T -- Really a blocking acquire.
  make Config {conn, key, ttl, renewInterval} = do
    value <- newUUID
    let connRaw = Redis.conn conn
        keyRaw = encodeUtf8 . untag $ key
        valueRaw = encodeUtf8 . untag $ value
        ttlRaw = toNum @Second @Integer ttl
    runRedis connRaw (acquire keyRaw valueRaw ttlRaw)
    let renewer = renew keyRaw valueRaw ttlRaw renewInterval
    renewThread <- async $ runRedis connRaw renewer
    return T {conn, key, value, renewThread}
  cleanup :: T -> IO ()
  cleanup T {conn, key, value, renewThread} = do
    let connRaw = Redis.conn conn
        keyRaw = encodeUtf8 . untag $ key
        valueRaw = encodeUtf8 . untag $ value
    cancel renewThread
    runRedis connRaw (delete keyRaw valueRaw)
