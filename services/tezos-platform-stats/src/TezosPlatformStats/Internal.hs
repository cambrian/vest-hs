module TezosPlatformStats.Internal
  ( module TezosPlatformStats.Internal
  ) where

import qualified Amqp
import qualified Postgres
import qualified TezosChainWatcher.Internal as TezosChainWatcher
import qualified TezosPlatformCore.Internal as TezosPlatformCore
import Vest
import qualified WebSocket

data T = T
  { webSocket :: WebSocket.T
  , amqp :: Amqp.T
  , coreDb :: Pool (Specific TezosPlatformCore.T Postgres.Connection)
  , chainWatcherDb :: Pool (Specific TezosChainWatcher.T Postgres.Connection)
  , redis :: RedisConnection
  }

instance HasNamespace T where
  type Namespace T = "tezos-platform-stats"

instance Has WebSocket.T T where
  get = webSocket

instance Has Amqp.T T where
  get = amqp

instance Has RedisConnection T where
  get = redis

instance Has (Pool (Specific TezosPlatformCore.T Postgres.Connection)) T where
  get = coreDb

instance Has (Pool (Specific TezosChainWatcher.T Postgres.Connection)) T where
  get = chainWatcherDb
