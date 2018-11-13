module DummyManager.Internal
  ( module DummyManager.Internal
  ) where

import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { webSocket :: WebSocket.T
  } deriving (HasNamespace)

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket
