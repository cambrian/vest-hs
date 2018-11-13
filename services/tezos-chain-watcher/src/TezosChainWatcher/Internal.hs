module TezosChainWatcher.Internal
  ( module TezosChainWatcher.Internal
  ) where

import qualified AccessControl.Client
import qualified Db
import qualified Http
import qualified Tezos
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { dbPool :: Pool Db.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Http.T
  , accessControlClient :: AccessControl.Client.T
  , lastProducedBlockNumber :: TMVar Word64
  , lastConsumedBlockNumber :: TMVar Word64
  , blockEventConsumerWriter :: StreamWriter Tezos.BlockEvent
  , blockEventConsumerStream :: Stream QueueBuffer Tezos.BlockEvent
  } deriving (HasNamespace)

instance HasRedisConnection T where
  redisConnection = redis

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient

instance Db.HasConnection T where
  withConnection t = withResource $ dbPool t
