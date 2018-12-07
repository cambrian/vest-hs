module TezosPlatformStats.Api
  ( module TezosPlatformStats.Api
  ) where

import qualified Tezos
import TezosPlatformStats.Internal
import Vest
import qualified WebSocket

type OriginationsByImplicitAddressEndpoint
   = EndpointJson 'NoAuth T WebSocket.T "originationsByImplicitAddress" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])

type BakerCountEndpoint
   = EndpointJson 'NoAuth T WebSocket.T "bakerCount" () ('Direct Int)

testOriginationsByImplicitAddressEndpoint ::
     WebSocket.T -> IO [Tezos.OriginatedAddress]
testOriginationsByImplicitAddressEndpoint websocket = do
  let bindings =
        makeClient
          websocket
          (Proxy :: Proxy OriginationsByImplicitAddressEndpoint)
  res <- bindings (Tagged $ Tagged "tz1Wit2PqodvPeuRRhdQXmkrtU8e8bRYZecd")
  log
    Debug
    "originations for implicit address: tz1Wit2PqodvPeuRRhdQXmkrtU8e8bRYZecd"
    res
  return res

testBakerCountEndpoint :: WebSocket.T -> IO Int
testBakerCountEndpoint websocket = do
  let bindings = makeClient websocket (Proxy :: Proxy BakerCountEndpoint)
  res <- bindings ()
  log Debug "number of registered bakers" res
  return res
