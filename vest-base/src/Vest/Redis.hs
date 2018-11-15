module Vest.Redis
  ( defaultRedisConfig
  , RedisConfig(..)
  , RedisConnection
  , HasRedisConnection(..)
  , RedisTransactionException(..)
  ) where

import Database.Redis
import Database.Redis as Vest.Redis (ConnectInfo(..))
import qualified GHC.Base
import qualified Network.Socket.Internal as Internal (PortNumber(..))
import Vest.Loadable
import Vest.Prelude

data RedisTransactionException =
  RedisTransactionException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

type RedisConnection = Connection

class HasRedisConnection t where
  redisConnection :: t -> Connection

instance HasRedisConnection RedisConnection where
  redisConnection = identity

data RedisConfig = RedisConfig
  { host :: GHC.Base.String
  , port :: Word16
  , auth :: Maybe ByteString
  } deriving (Generic, FromJSON)

defaultRedisConfig :: RedisConfig
defaultRedisConfig =
  RedisConfig {host = "localhost", port = 6379, auth = Nothing}

toPortId :: (Integral a) => a -> PortID
toPortId port = PortNumber (fromIntegral port :: Internal.PortNumber)

toConnectInfo :: RedisConfig -> ConnectInfo
toConnectInfo RedisConfig {host, port, auth} =
  defaultConnectInfo
    {connectHost = host, connectPort = toPortId port, connectAuth = auth}

instance Resource Connection where
  type ResourceConfig Connection = RedisConfig
  make :: RedisConfig -> IO Connection
  make = checkedConnect . toConnectInfo
  cleanup :: Connection -> IO ()
  cleanup = disconnect

instance Loadable Connection where
  configName = "redis"
