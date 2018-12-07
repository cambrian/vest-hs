module TezosChainWatcher.Internal
  ( module TezosChainWatcher.Internal
  ) where

import qualified AccessControl.Client
import qualified Amqp
import qualified Postgres
import qualified Tezos
import qualified Tezos.Rpc
import Vest

newtype DefaultOperationFee = DefaultOperationFee
  { mutez :: Integer
  } deriving (Eq, Show, Generic) deriving anyclass (FromJSON)

instance Loadable DefaultOperationFee where
  configFile = [relfile|default-operation-fee.yaml|]

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Rpc.T
  , accessControlClient :: AccessControl.Client.T
  , finalizedHeightStream :: Stream ValueBuffer Word64
  , provisionalBlockEventStream :: Stream QueueBuffer Tezos.BlockEvent
  , defaultOperationFee :: FixedQty XTZ
  }

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
