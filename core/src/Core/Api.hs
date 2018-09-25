module Core.Api
  ( module Core.Api
  ) where

import qualified Bridge
import VestPrelude
import qualified VestPrelude.Money as Money

data VirtualStakeRequest (a :: Symbol) = VirtualStakeRequest
  { user :: Id "Address"
  , size :: Money.Dense a
  , duration :: Time Day
  , payment :: ()
  } deriving (Eq, Read, Show)

data VirtualStakeResponse (a :: Symbol) = VirtualStakeResponse
  { id :: Id "VirtualStake"
  , size :: Money.Dense a
  , duration :: Time Day
  , payment :: ()
  } deriving (Eq, Read, Show)

type VirtualStakeRoute currency = AppendSymbol "virtualStake/" currency

type VirtualStakeEndpoint currency
   = Bridge.DirectEndpoint (VirtualStakeRoute currency) (VirtualStakeRequest currency) (Money.Dense "USD")
