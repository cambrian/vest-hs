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
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Rpc.T
  , accessControlClient :: AccessControl.Client.T
  , lastConsumedFinalBlockNumber :: TVar Word64
  , lastConsumedBlockProvisionalId :: TVar Int
  , maxConsumedProvisionalBlockNumber :: TVar Word64
  , finalizationLag :: TezosFinalizationLag
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
