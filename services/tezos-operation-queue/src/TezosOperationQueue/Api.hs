module TezosOperationQueue.Api
  ( module TezosOperationQueue.Api
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Permission as Permission
import qualified Amqp
import qualified Tezos
import qualified TezosOperationQueue.Internal as TezosOperationQueue
import Vest
import qualified WebSocket

-- | Public endpoint for browsers to hit
type InjectEndpoint
   = EndpointJson 'NoAuth TezosOperationQueue.T WebSocket.T "inject" Tezos.SignedOperation ('Direct ())

-- | Higher priority inject endpoint for internal use
type InjectVestEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.InjectOperation)) TezosOperationQueue.T Amqp.T "injectVest" Tezos.SignedOperation ('Direct Tezos.OperationHash)
