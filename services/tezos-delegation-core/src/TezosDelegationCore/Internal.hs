module TezosDelegationCore.Internal
  ( module TezosDelegationCore.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { dbPool :: Pool Postgres.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-delegation-core"

instance Postgres.HasConnection T where
  withConnection t = withResource $ dbPool t

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis

instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient
