module Vest.Redis
  ( module Vest.Redis
  ) where

import Database.Redis
import Vest.Prelude

type RedisConnection = Connection

localRedisConfig :: ConnectInfo
localRedisConfig = defaultConnectInfo

data RedisTransactionException =
  RedisTransactionException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

class HasRedisConnection t where
  redisConnection :: t -> Connection

instance Resource Connection where
  type ResourceConfig Connection = ConnectInfo
  make :: ConnectInfo -> IO Connection
  make = checkedConnect
  cleanup :: Connection -> IO ()
  cleanup = disconnect
