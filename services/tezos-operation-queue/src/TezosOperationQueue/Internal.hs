module TezosOperationQueue.Internal
  ( module TezosOperationQueue.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-operation-queue"

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
