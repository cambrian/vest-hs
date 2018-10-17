module Tezos.TzScan
  ( module Tezos.TzScan
  ) where

import Vest
import Vest.Http

type V1 = "v1"

type V2 = "v2"

type Direct result = Get '[ JSON] result

data AddressTz = AddressTz
  { tz :: Text
  } deriving (Show, Generic, FromJSON)

data RewardsSplit = RewardsSplit
  { delegate_staking_balance :: Text
  , delegators_nb :: Int
  , delegators_balance :: [(AddressTz, Text)]
  , blocks_rewards :: Int
  , endorsements_rewards :: Int
  , fees :: Int
  , future_blocks_rewards :: Int
  , future_endorsements_rewards :: Int
  , gain_from_denounciation :: Int
  , lost_deposit_from_denounciation :: Int
  , lost_rewards_denounciation :: Int
  , lost_fees_denounciation :: Int
  } deriving (Show, Generic, FromJSON)

type GetRewardsSplit
   = V2 :> "rewards_split" :> Capture "delegate" Text :> QueryParam' '[ Required] "cycle" Int :> QueryParam' '[ Required] "p" Int :> Direct RewardsSplit
