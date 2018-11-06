module TezosOperationQueue.Api
  ( module TezosOperationQueue.Api
  ) where

import qualified TezosOperationQueue.Internal as TezosOperationQueue
import qualified Transport.WebSocket as WebSocket
import Vest

type InjectEndpoint
   = EndpointJson 'NoAuth TezosOperationQueue.T WebSocket.T "inject" (Text' "TzOperation") ('Direct ())
