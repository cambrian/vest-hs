module TezosChainWatcher.Internal
  ( module TezosChainWatcher.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Tezos
import qualified Tezos.Rpc
import qualified Transport.Amqp as Amqp
import Vest

newtype TezosFinalizationLag =
  TezosFinalizationLag Int
  deriving newtype (FromJSON)

type ProvisionalEvent = (Int, Tezos.BlockEvent)

data T = T
  { dbPool :: Pool Postgres.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Rpc.T
  , accessControlClient :: AccessControl.Client.T
  , lastConsumedFinalBlockNumber :: TVar Word64
  , blockEventConsumerWriter :: StreamWriter Tezos.BlockEvent
  , blockEventConsumerStream :: Stream QueueBuffer Tezos.BlockEvent
  , lastConsumedBlockProvisionalId :: TVar Int
  , maxConsumedProvisionalBlockNumber :: TVar Word64
  , provisionalEventConsumerWriter :: StreamWriter ProvisionalEvent
  , provisionalEventConsumerStream :: Stream QueueBuffer ProvisionalEvent
  , finalizationLag :: TezosFinalizationLag
  }

instance Loadable TezosFinalizationLag where
  configFile = [relfile|tezos-finalization-lag.yaml|]

instance HasNamespace T where
  type Namespace T = "tezos-chain-watcher"

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
