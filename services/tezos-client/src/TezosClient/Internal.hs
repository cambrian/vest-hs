module TezosClient.Internal
  ( module TezosClient.Internal
  ) where

import qualified Http
import qualified Transport.WebSocket as WebSocket
import Vest

data T = T
  { webSocket :: WebSocket.T
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , connection :: Http.T
  }

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket
