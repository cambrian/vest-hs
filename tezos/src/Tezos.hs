module Tezos
  ( module Tezos
  ) where

import Data.Aeson
import qualified Http
import Tezos.Node
import Vest

data UnexpectedResultException =
  UnexpectedResultException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

data DelegationInfo = DelegationInfo
  { delegator :: Text' "TzOriginatedPkh"
  , size :: FixedQty' "DelegationSize" "XTZ"
  } deriving (Show, Generic, FromJSON)

data RewardInfo = RewardInfo
  { delegate :: Text' "TzImplicitPkh"
  , reward :: FixedQty' "RewardSize" "XTZ"
  , staked :: FixedQty' "StakedSize" "XTZ"
  , delegations :: [DelegationInfo]
  } deriving (Show, Generic, FromJSON)

toFixedQtyUnsafe :: Text -> IO (FixedQty' t "XTZ")
toFixedQtyUnsafe x = readUnsafe @Integer x >>- fromInteger

-- TODO: Debug discrepancies between this and TzScan.
-- Change: blockInCycle -> lastBlockInCycle
-- Change: Use cycle - 7 block for correct rewards.
getCycleRewardInfo ::
     Http.T -> Text' "TzBlockHash" -> Text' "TzImplicitPkh" -> IO RewardInfo
getCycleRewardInfo connection (Tagged blockInCycle) (Tagged delegateId) = do
  delegate <-
    Http.direct
      (Http.request
         (Proxy :: Proxy GetDelegate)
         mainChain
         blockInCycle
         delegateId)
      connection
  frozenBalance <-
    fromJustUnsafe
      UnexpectedResultException
      (last (frozen_balance_by_cycle delegate))
  reward <- toFixedQtyUnsafe (rewards frozenBalance)
  staked <- toFixedQtyUnsafe (staking_balance delegate)
  let delegators = delegated_contracts delegate
  delegations <-
    mapConcurrently
      (\delegator -> do
         size <-
           Http.direct
             (Http.request
                (Proxy :: Proxy GetContractBalance)
                mainChain
                blockInCycle
                delegator)
             connection >>=
           toFixedQtyUnsafe
         return DelegationInfo {delegator = Tagged delegator, size})
      delegators
  return RewardInfo {delegate = Tagged delegateId, reward, staked, delegations}
