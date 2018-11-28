module DummyManager.Internal
  ( module DummyManager.Internal
  ) where

import Vest
import qualified WebSocket

data T = T
  { webSocket :: WebSocket.T
  , redis :: RedisConnection
  }

instance HasNamespace T where
  type Namespace T = "dummy-manager"

instance Has WebSocket.T T where
  get = webSocket

instance Has RedisConnection T where
  get = redis
