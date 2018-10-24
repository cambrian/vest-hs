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

snapshotLag :: Int -> Int
snapshotLag = subtract 2

toFixedQtyUnsafe :: Text -> IO (FixedQty' t "XTZ")
toFixedQtyUnsafe x = readUnsafe @Integer x >>- fromInteger

-- | Calculates reward split info.
-- Assume that the current cycle is X.
-- rewardBlock: The last block in cycle (X - frozenCycles).
-- snapshotBlock: The snapshot block in cycle (X - frozenCycles - snapshotLag).
getRewardInfo ::
     Http.T
  -> Text' "TzBlockHash"
  -> Text' "TzBlockHash"
  -> Text' "TzImplicitPkh"
  -> IO RewardInfo
getRewardInfo connection (Tagged rewardBlock) (Tagged snapshotBlock) (Tagged delegateId) = do
  frozenBalanceCycles <-
    Http.direct
      (Http.request
         (Proxy :: Proxy ListFrozenBalanceCycles)
         mainChain
         rewardBlock
         delegateId)
      connection
  frozenBalance <-
    fromJustUnsafe UnexpectedResultException (last frozenBalanceCycles)
  reward <-
    (+) <$> toFixedQtyUnsafe (rewards frozenBalance) <*>
    toFixedQtyUnsafe (fees frozenBalance)
  delegateSnapshot <-
    Http.direct
      (Http.request
         (Proxy :: Proxy GetDelegate)
         mainChain
         snapshotBlock
         delegateId)
      connection
  staked <- toFixedQtyUnsafe (staking_balance delegateSnapshot)
  let delegators = delegated_contracts delegateSnapshot
  delegations <-
    mapConcurrently
      (\delegator -> do
         size <-
           Http.direct
             (Http.request
                (Proxy :: Proxy GetContractBalance)
                mainChain
                snapshotBlock
                delegator)
             connection >>=
           toFixedQtyUnsafe
         return DelegationInfo {delegator = Tagged delegator, size})
      delegators
  return RewardInfo {delegate = Tagged delegateId, reward, staked, delegations}
