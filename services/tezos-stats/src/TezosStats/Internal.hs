module TezosStats.Internal
  ( module TezosStats.Internal
  ) where

import Vest
import qualified WebSocket

data T = T
  { webSocket :: WebSocket.T
  , redis :: RedisConnection
  , rawStubData :: Text
  , streamDelayMillis :: Natural
  }

instance HasNamespace T where
  type Namespace T = "tezos-stats"

instance Has WebSocket.T T where
  get = webSocket

instance Has RedisConnection T where
  get = redis
