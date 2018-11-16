module TezosHotWallet.Internal
  ( module TezosHotWallet.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Tezos.Node
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { dbPool :: Pool Postgres.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Tezos.Node.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-hot-wallet"

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
