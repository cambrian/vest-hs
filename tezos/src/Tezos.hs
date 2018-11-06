module Tezos
  ( module Tezos
  , module Reexports
  ) where

import Data.Aeson
import qualified Http
import Tezos.Node
import Tezos.Prelude as Reexports
import Vest

data UnexpectedResultException =
  UnexpectedResultException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

snapshotLag :: Int -> Int
snapshotLag = subtract 7

toFixedQtyUnsafe :: Text -> IO (FixedQty XTZ)
toFixedQtyUnsafe x = readUnsafe @Integer x >>- fromInteger

-- | Calculates reward split info.
-- Assume that the current cycle is X.
-- rewardBlock: The last block in cycle (X - 1 - frozenCycles).
-- snapshotBlock: The snapshot block in cycle (X - 1 - frozenCycles - snapshotLag).
-- These rewards are to be paid out during the current cycle.
getRewardInfo ::
     Http.T -> BlockHash -> BlockHash -> ImplicitAccount -> IO RewardInfo
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
-- materializeBlockEvent :: Http.T -> IndexOf BlockEvent -> IO BlockEvent
-- materializeBlockEvent connection index = do
--   latestBlock <- Http.direct
--       (Http.request
--          (Proxy :: Proxy GetBlock)
--          mainChain
--          headBlock)
--       connection
--   blockLevel <-
