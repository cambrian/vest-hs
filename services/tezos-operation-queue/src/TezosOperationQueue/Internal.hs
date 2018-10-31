module TezosOperationQueue.Internal
  ( module TezosOperationQueue.Internal
  ) where

import qualified Db
import qualified Http
import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { db :: Db.Connection
  , webSocket :: WebSocket.T
  , tezos :: Http.T
  }

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket
