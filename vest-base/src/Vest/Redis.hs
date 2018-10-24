module Vest.Redis
  ( module Vest.Redis
  ) where

import Database.Redis
import Database.Redis as Vest.Redis (ConnectInfo(..))
import qualified Network.Socket.Internal as Internal (PortNumber(..))
import Vest.Prelude

type RedisConnection = Connection

defaultRedisConfig :: ConnectInfo
defaultRedisConfig = defaultConnectInfo

toPortId :: (Integral a) => a -> PortID
toPortId port = PortNumber (fromIntegral port :: Internal.PortNumber)

data RedisTransactionException =
  RedisTransactionException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

class HasRedisConnection t where
  redisConnection :: t -> Connection

instance HasRedisConnection RedisConnection where
  redisConnection = identity

instance Resource Connection where
  type ResourceConfig Connection = ConnectInfo
  make :: ConnectInfo -> IO Connection
  make = checkedConnect
  cleanup :: Connection -> IO ()
  cleanup = disconnect
