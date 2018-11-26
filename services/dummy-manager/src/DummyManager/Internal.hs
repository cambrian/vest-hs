module DummyManager.Internal
  ( module DummyManager.Internal
  ) where

import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { webSocket :: WebSocket.T
  }

instance HasNamespace T where
  type Namespace T = "dummy-manager"

instance Has WebSocket.T T where
  get = webSocket
