module TezosPlatformStats.Api
  ( module TezosPlatformStats.Api
  ) where

import qualified Tezos
import TezosPlatformStats.Internal
import Vest
import qualified WebSocket

type OriginationsByImplicitAddressEndpoint
   = EndpointJson 'NoAuth T WebSocket.T "originationsByImplicitAddress" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])

testEndpoint :: WebSocket.T -> IO [Tezos.OriginatedAddress]
testEndpoint websocket = do
  let binding =
        makeClient
          websocket
          (Proxy :: Proxy OriginationsByImplicitAddressEndpoint)
  res <- binding (Tagged $ Tagged "tz1Wit2PqodvPeuRRhdQXmkrtU8e8bRYZecd")
  log
    Debug
    "originations for implicit address: tz1Wit2PqodvPeuRRhdQXmkrtU8e8bRYZecd"
    res
  return res
