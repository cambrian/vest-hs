module TezosHotWallet.Api
  ( module TezosHotWallet.Api
  ) where

import qualified AccessControl.Auth
import qualified AccessControl.Permission as Permission
import qualified Tezos
import TezosHotWallet.Internal
import qualified Transport.Amqp as Amqp
import Vest

data PaymentEvent = PaymentEvent
  { idx :: Word64
  , hash :: Tezos.OperationHash
  , from :: Tezos.Address
  , size :: FixedQty XTZ
  , time :: Time
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable PaymentEvent where
  type IndexOf PaymentEvent = Word64
  index = idx

type PaymentEvents = Event T Amqp.T "payments" PaymentEvent

data PayoutRequest = PayoutRequest
  { id :: UUID
  , to :: Tezos.Address
  , size :: FixedQty XTZ
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

type PayoutEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.IssuePayout)) T Amqp.T "payout" PayoutRequest ('Direct ())
