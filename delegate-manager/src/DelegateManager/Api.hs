module DelegateManager.Api
  ( module DelegateManager.Api
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import VestPrelude
import qualified VestPrelude.Money as Money

data VirtualStakeRequest (currency :: Symbol) (unit :: Symbol) = VirtualStakeRequest
  { user :: Text' "Address"
  , size :: Money.Discrete currency unit
  , duration :: Time Day
  , payment :: ()
  } deriving (Generic)

deriving instance Money.Unit c u => Read (VirtualStakeRequest c u)

deriving instance Money.Unit c u => Show (VirtualStakeRequest c u)

instance Money.Unit c u => ToJSON (VirtualStakeRequest c u)

instance Money.Unit c u => FromJSON (VirtualStakeRequest c u)

data VirtualStakeResponse (currency :: Symbol) (unit :: Symbol) = VirtualStakeResponse
  { id :: Text' "VirtualStakeId"
  , owner :: Text' "Address"
  , size :: Money.Discrete currency unit
  , duration :: Time Day
  , startTime :: Timestamp
  , price :: Money.Discrete "USD" "cent"
  } deriving (Generic)

deriving instance Money.Unit c u => Read (VirtualStakeResponse c u)

deriving instance Money.Unit c u => Show (VirtualStakeResponse c u)

instance Money.Unit c u => ToJSON (VirtualStakeResponse c u)

instance Money.Unit c u => FromJSON (VirtualStakeResponse c u)

type VirtualStakeRoute currency = AppendSymbol "virtualStake/" currency

type VirtualStakeEndpoint c u
   = ( Endpoint 'NoAuth (VirtualStakeRoute c) (VirtualStakeRequest c u) ('Direct (VirtualStakeResponse c u))
     , Amqp.T)
