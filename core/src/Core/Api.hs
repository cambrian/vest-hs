module Core.Api
  ( module Core.Api
  ) where

import Bridge
import VestPrelude
import qualified VestPrelude.Money as Money

data VirtualStakeRequest (currency :: Symbol) (unit :: Symbol) = VirtualStakeRequest
  { user :: Id "Address"
  , size :: Money.Discrete currency unit
  , duration :: Time Day
  , payment :: ()
  } deriving (Generic)

deriving instance Money.Unit c u => Eq (VirtualStakeRequest c u)

deriving instance Money.Unit c u => Read (VirtualStakeRequest c u)

deriving instance Money.Unit c u => Show (VirtualStakeRequest c u)

deriving instance
         Money.Unit c u => ToJSON (VirtualStakeRequest c u)

deriving instance
         Money.Unit c u => FromJSON (VirtualStakeRequest c u)

data VirtualStakeResponse (currency :: Symbol) (unit :: Symbol) = VirtualStakeResponse
  { id :: Id "VirtualStake"
  , size :: Money.Discrete currency unit
  , duration :: Time Day
  , payment :: ()
  } deriving (Generic)

deriving instance Money.Unit c u => Eq (VirtualStakeResponse c u)

deriving instance Money.Unit c u => Read (VirtualStakeResponse c u)

deriving instance Money.Unit c u => Show (VirtualStakeResponse c u)

deriving instance
         Money.Unit c u => ToJSON (VirtualStakeResponse c u)

deriving instance
         Money.Unit c u => FromJSON (VirtualStakeResponse c u)

type VirtualStakeRoute currency = AppendSymbol "virtualStake/" currency

type VirtualStakeEndpoint currency unit
   = Endpoint 'Direct 'NoAuth (VirtualStakeRoute currency) (VirtualStakeRequest currency unit) (VirtualStakeResponse currency unit)
