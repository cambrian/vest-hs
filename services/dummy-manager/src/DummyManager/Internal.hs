module DummyManager.Internal
  ( module DummyManager.Internal
  ) where

import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { webSocket :: WebSocket.T
  }

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasLogger T where
  logger _ = stderrLogger Warn
