module TezosInjector.Internal
  ( module TezosInjector.Internal
  ) where

import qualified AccessControl.Client
import qualified Amqp
import qualified Postgres
import qualified Tezos.Cli
import qualified Tezos.Rpc
import Vest
import qualified WebSocket

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  , tezosRpc :: Tezos.Rpc.T
  , tezosCli :: Tezos.Cli.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-injector"

instance Has (Pool (Specific T Postgres.Connection)) T where
  get = dbPool

instance Has Amqp.T T where
  get = amqp

instance Has WebSocket.T T where
  get = webSocket

instance Has RedisConnection T where
  get = redis

instance Has AccessControl.Client.T T where
  get = accessControlClient
