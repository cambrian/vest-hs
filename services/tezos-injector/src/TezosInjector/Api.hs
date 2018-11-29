module TezosInjector.Api
  ( module TezosInjector.Api
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Permission as Permission
import qualified Amqp
import qualified Tezos
import qualified TezosInjector.Internal as TezosInjector
import Vest
import qualified WebSocket

-- | Public operation injection endpoint for browsers to hit. Returns when operation has been
-- received and persisted.
type InjectEndpoint
   = EndpointJson 'NoAuth TezosInjector.T WebSocket.T "inject" Tezos.SignedOperation ('Direct ())

-- | Payout endpoint for Vest services to hit. Returns when logical operation has been received and
-- persisted.
type PayoutEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.IssuePayout)) TezosInjector.T Amqp.T "payout" (HashMap Tezos.Address (FixedQty XTZ)) ('Direct ())
