module Vest.DistLock
  ( defaultLock
  , Config(..)
  , T(..)
  ) where

import Database.Redis
import Vest.Prelude.Core hiding (get)
import Vest.Prelude.Resource
import Vest.Prelude.Time
import qualified Vest.Redis as Redis

-- | Works via the setnx command, which succeeds only if the desired key doesn't already exist.
-- Note that this algorithm will not work with several Redis masters (at which point we should
-- consider implementing the Redlock algorithm).
data T = T
  { conn :: Redis.T
  , lockId :: Text' "LockId"
  , cancelRenewer :: IO' "CancelInterval" ()
  }

data Config = Config
  { conn :: Redis.T
  , lockId :: Text' "LockId"
  , renewInterval :: Time Second
  }

data CacheStateException =
  CacheStateException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

defaultLock :: Redis.T -> Text' "LockId" -> ResourceConfig T
defaultLock conn lockId = Config {conn, lockId, renewInterval = sec 5}

acquire :: ByteString -> Integer -> Time Second -> Redis ()
-- ^ Polls every pollInterval until lock is acquired. Unfortunately there is no guaranteed way to
-- wake on-demand using event notifications (PubSub delivery is best-effort), and Redis lacks a
-- number of useful blocking calls.
acquire key ttl pollInterval = do
  txResult <-
    multiExec $ do
      acquired <- setnx key "0"
      expired <- expire key ttl
       -- ^ setnx does not set any TTL on its own.
      return $ (&&) <$> acquired <*> expired
  case txResult of
    TxSuccess True -> return ()
    TxSuccess _ -> threadDelay pollInterval >> acquire key ttl pollInterval
    _ -> liftIO $ throw Redis.TransactionException

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
  -- ^ Blocks until lock is acquired.
  make Config {conn, lockId, renewInterval} = do
    let connRaw = Redis.conn conn
        keyRaw = encodeUtf8 $ untag lockId
        ttlRaw = toNum @Second @Integer (2 *:* renewInterval)
    -- Default pollInterval is just the renewInterval.
    runRedis connRaw (acquire keyRaw ttlRaw renewInterval)
    (_, cancelRenewer) <-
      intervalRenewable
        renewInterval
        (void . runRedis connRaw $ expire keyRaw ttlRaw)
    return T {conn, lockId, cancelRenewer}
  cleanup :: T -> IO ()
  cleanup T {conn, lockId, cancelRenewer} = do
    let connRaw = Redis.conn conn
        keyRaw = encodeUtf8 . untag $ lockId
    untag cancelRenewer
    void . runRedis connRaw $ del [keyRaw]
