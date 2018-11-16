module TezosOperationQueue.Internal
  ( module TezosOperationQueue.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { dbPool :: Pool Postgres.Connection
  , amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-operation-queue"

instance Postgres.HasConnection T where
  withConnection t = withResource $ dbPool t

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasRedisConnection T where
  redisConnection = redis

instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient
