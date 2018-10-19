module Tezos
  ( module Tezos
  ) where

import Data.Aeson
import qualified Http
import Tezos.TzScan
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

convertTzScanDelegations :: AddressTz -> Value -> IO Delegation
convertTzScanDelegations addressTz delSizeValue = do
  delSize <- fromValueUnsafe delSizeValue
  return
    Delegation {delegator = Tagged $ tz addressTz, size = fromInteger delSize}

getRewardSplit :: Http.T -> Text' "TzImplicitPkh" -> Int -> IO RewardSplit
getRewardSplit connection (Tagged delegateAddr) cycle = do
  let getPage page =
        Http.direct
          (Http.request (Proxy :: Proxy GetRewardsSplit) delegateAddr cycle page)
          connection
  rewardInfo <- getPage 0
  let paginate numDels page = do
        pageRewards <- getPage page
        delegations <-
          mapM
            (uncurry convertTzScanDelegations)
            (delegators_balance pageRewards)
        let remaining = numDels - length delegations
        if remaining == 0
          then return delegations
          else paginate remaining (page + 1) >>= \ds ->
                 return (delegations ++ ds)
  delegations <- paginate (delegators_nb rewardInfo) 0
  stakedInteger <- fromValueUnsafe $ delegate_staking_balance rewardInfo
  blockRewards <- fromValueUnsafe $ blocks_rewards rewardInfo
  endorsementRewards <- fromValueUnsafe $ endorsements_rewards rewardInfo
  return
    RewardSplit
      { delegate = Tagged delegateAddr
      , reward = Tagged . fromInteger $ blockRewards + endorsementRewards
      , staked = Tagged . fromInteger $ stakedInteger
      , delegations
      }
