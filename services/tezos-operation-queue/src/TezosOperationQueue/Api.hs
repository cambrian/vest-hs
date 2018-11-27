module TezosOperationQueue.Api
  ( module TezosOperationQueue.Api
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Permission as Permission
import qualified Tezos
import qualified TezosOperationQueue.Internal as TezosOperationQueue
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

type InjectEndpoint
   = EndpointJson 'NoAuth TezosOperationQueue.T WebSocket.T "inject" Tezos.SignedOperation ('Direct ())

-- | Higher priority inject endpoint for internal use
type InjectVestEndpoint
   = EndpointJson ('Auth (AccessControl.Auth.T 'Permission.InjectOperation)) TezosOperationQueue.T Amqp.T "injectVest" Tezos.SignedOperation ('Direct Tezos.OperationHash)
