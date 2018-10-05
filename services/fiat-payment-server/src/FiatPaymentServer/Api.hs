module FiatPaymentServer.Api
  ( module FiatPaymentServer.Api
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import VestPrelude
import qualified VestPrelude.Money as Money

data ChargeRequest (currency :: Symbol) (unit :: Symbol) = ChargeRequest
  { size :: Money.Discrete currency unit
  , token :: Text' "PaymentToken"
  } deriving (Generic)

deriving instance Money.Unit c u => Read (ChargeRequest c u)

deriving instance Money.Unit c u => Show (ChargeRequest c u)

instance Money.Unit c u => ToJSON (ChargeRequest c u)

instance Money.Unit c u => FromJSON (ChargeRequest c u)

data ChargeException =
  ChargeException
  deriving (Read, Show, ToJSON, FromJSON, Generic, Exception)

-- data Charge (currency :: Symbol) (unit :: Symbol) = Charge
--   { size :: Money.Discrete currency unit
--   , time :: Timestamp
--   } deriving (Generic)
type ChargeResponse = Either ChargeException ()

-- deriving instance Money.Unit c u => Read (ChargeResponse c u)
-- deriving instance Money.Unit c u => Show (ChargeResponse c u)
-- instance Money.Unit c u => ToJSON (ChargeResponse c u)
-- instance Money.Unit c u => FromJSON (ChargeResponse c u)
type ChargeRoute currency = AppendSymbol "charge/" currency

type ChargeEndpoint c u
   = ( Endpoint 'NoAuth (ChargeRoute c) (ChargeRequest c u) ('Direct ChargeResponse)
     , Amqp.T)
