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

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Rpc.T
  , accessControlClient :: AccessControl.Client.T
  , finalizedBlockEventStream :: Stream QueueBuffer Tezos.BlockEvent
  , provisionalBlockEventStream :: Stream QueueBuffer Tezos.BlockEvent
  , finalizedHeightStream :: Stream ValueBuffer Word64
  }

instance Loadable TezosFinalizationLag where
  configFile = [relfile|tezos-finalization-lag.yaml|]

instance HasNamespace T where
  type Namespace T = "tezos-chain-watcher"

instance Has (Pool (Specific T Postgres.Connection)) T where
  get = dbPool

instance Has Amqp.T T where
  get = amqp

instance Has RedisConnection T where
  get = redis

instance Has AccessControl.Client.T T where
  get = accessControlClient
