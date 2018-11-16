module TezosChainWatcher.Internal
  ( module TezosChainWatcher.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Tezos
import qualified Tezos.Rpc
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { dbPool :: Pool Postgres.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Rpc.T
  , accessControlClient :: AccessControl.Client.T
  , lastConsumedBlockNumber :: TMVar Word64
  , blockEventConsumerWriter :: StreamWriter Tezos.BlockEvent
  , blockEventConsumerStream :: Stream QueueBuffer Tezos.BlockEvent
  }

instance HasNamespace T where
  type Namespace T = "tezos-chain-watcher"

instance HasRedisConnection T where
  redisConnection = redis

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient

instance Postgres.HasConnection T where
  withConnection t = withResource $ dbPool t
