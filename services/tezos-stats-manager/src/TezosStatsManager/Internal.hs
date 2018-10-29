module TezosStatsManager.Internal
  ( module TezosStatsManager.Internal
  ) where

import qualified AccessControl.Client as AccessControlClient
import qualified Db
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { db :: Db.Connection
  , amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  , accessControlClient :: AccessControlClient.T
  }

instance HasRedisConnection T where
  redisConnection = redis

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance AccessControlClient.Has T where
  accessControlClient = accessControlClient
