module TezosPlatformStats.Api
  ( module TezosPlatformStats.Api
  ) where

import qualified Tezos
import TezosPlatformStats.Internal
import Vest
import qualified WebSocket

type OriginationsByImplicitAddressEndpoint
   = EndpointJson 'NoAuth T WebSocket.T "originationsByImplicitAddress" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])
