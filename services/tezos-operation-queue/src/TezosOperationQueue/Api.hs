module TezosOperationQueue.Api
  ( module TezosOperationQueue.Api
  ) where

import qualified Tezos
import qualified TezosOperationQueue.Internal as TezosOperationQueue
import qualified Transport.WebSocket as WebSocket
import Vest

type InjectEndpoint
   = EndpointJson 'NoAuth TezosOperationQueue.T WebSocket.T "inject" Tezos.SignedOperationContents ('Direct ())
