module TezosOperationQueue.Internal
  ( module TezosOperationQueue.Internal
  ) where

-- import qualified Db
import qualified Transport.WebSocket as WebSocket

-- import qualified AccessControl.Client as AccessControlClient
-- import qualified Transport.Amqp as Amqp
import Vest

data T = T
    -- db :: Db.Connection
  -- , amqp :: Amqp.T
  { webSocket :: WebSocket.T
  -- , redis :: RedisConnection
  -- , accessControlClient :: AccessControlClient.T
  }

instance HasNamespace T where
  type Namespace T = "tezos-operation-queue"

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket
