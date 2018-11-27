module TezosHotWallet.Api
  ( module TezosHotWallet.Api
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Permission as Permission
import qualified Tezos
import TezosHotWallet.Internal
import TezosHotWallet.Internal as TezosHotWallet.Api (PaymentEvent(..))
import qualified Transport.Amqp as Amqp
import Vest

type PaymentEvents = Event T Amqp.T "payments" PaymentEvent

data PayoutRequest = PayoutRequest
  { id :: UUID
  , to :: Tezos.Address
  , size :: FixedQty XTZ
  , fee :: FixedQty XTZ
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

type PayoutEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.IssuePayout)) T Amqp.T "payout" PayoutRequest ('Direct ())
