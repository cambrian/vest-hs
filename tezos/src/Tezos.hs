module Tezos
  ( module Tezos
  ) where

import Data.Aeson
import Vest

data Delegation = Delegation
  { delegator :: Text' "TzOriginatedPkh"
  , size :: FixedQty' "DelegationSize" "XTZ"
  } deriving (Show, Generic, FromJSON)

data RewardSplit = RewardSplit
  { delegate :: Text' "TzImplicitPkh"
  , reward :: FixedQty' "RewardSize" "XTZ"
  , staked :: FixedQty' "StakedSize" "XTZ"
  , delegations :: [Delegation]
  } deriving (Show, Generic, FromJSON)
