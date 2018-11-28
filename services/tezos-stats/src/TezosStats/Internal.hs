module TezosStats.Internal
  ( module TezosStats.Internal
  ) where

import Vest

-- import qualified AccessControl.Client as AccessControlClient
-- import qualified Db
-- import qualified Amqp
import qualified WebSocket

data T = T
    -- db :: Db.Connection
  -- , amqp :: Amqp.T
  { webSocket :: WebSocket.T
  , redis :: RedisConnection
  -- , accessControlClient :: AccessControlClient.T
  , rawStubData :: Text
  , streamDelayMillis :: Natural
  }

instance HasNamespace T where
  type Namespace T = "tezos-stats"

instance Has WebSocket.T T where
  get = webSocket

instance Has RedisConnection T where
  get = redis
-- instance HasRpcTransport Amqp.T T where
--   rpcTransport = amqp
-- instance AccessControlClient.Has T where
--   accessControlClient = accessControlClient
