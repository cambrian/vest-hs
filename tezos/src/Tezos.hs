module Tezos
  ( module Reexports
  , getRewardInfo
  , UnexpectedResultException(..)
  ) where

import Data.Aeson
import qualified Http
import Tezos.Node
import Tezos.Prelude as Reexports
import Vest

data UnexpectedResultException =
  UnexpectedResultException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

toFixedQtyUnsafe :: Text -> IO (FixedQty XTZ)
toFixedQtyUnsafe x = readUnsafe @Integer x >>- fromInteger

-- If Tezos ever hard-forks, our hard-coded constants might break. For now, they make this library
-- significantly easier to implement.
-- TODO: How to use ratios here?
blocksPerCycle :: Int
blocksPerCycle = 4096

blocksPerSnapshot :: Int
blocksPerSnapshot = 256

firstBlockNumber :: Int
firstBlockNumber = 1

frozenCycles :: Int
frozenCycles = 5

snapshotLag :: Int
snapshotLag = 7

firstBlockNumberInCycle :: Int -> Int
firstBlockNumberInCycle cycleNumber =
  firstBlockNumber + cycleNumber * blocksPerCycle

lastBlockNumberInCycle :: Int -> Int
lastBlockNumberInCycle cycleNumber =
  firstBlockNumberInCycle (cycleNumber + 1) - 1

snapshotOffset :: Int -> Int
snapshotOffset snapshotIndex = (snapshotIndex + 1) * blocksPerSnapshot - 1

-- | Baking cycle for rewards unfrozen in the given cycle.
bakingCycle :: Int -> Int
bakingCycle cycleNumber = cycleNumber - 1 - frozenCycles

-- | Snapshot cycle for rewards unfrozen in the given cycle.
snapshotCycle :: Int -> Int
snapshotCycle cycleNumber = bakingCycle cycleNumber - snapshotLag

-- | Will throw if targetBlockNumber is in the future.
getBlockHash :: Http.T -> Int -> IO BlockHash
getBlockHash connection targetBlockNumber = do
  latestBlock <-
    Http.direct
      (Http.request (Proxy :: Proxy GetBlock) mainChain headBlockHash)
      connection
  -- Ugly let because of duplicate record fields.
  let Block {hash = latestBlockHash} = latestBlock
      BlockHeader {level = latestBlockNumber} = header latestBlock
      offset = latestBlockNumber - targetBlockNumber
  when (offset < 0) $ throw UnexpectedResultException
  Tagged <$>
    Http.direct
      (Http.request
         (Proxy :: Proxy GetBlockHash)
         mainChain
         (latestBlockHash <> "~" <> show offset))
      connection

getLastBlockHash :: Http.T -> Int -> IO BlockHash
getLastBlockHash connection cycleNumber =
  getBlockHash connection (lastBlockNumberInCycle cycleNumber)

-- | Takes a while to run, but we only have to run this once per cycle.
getSnapshotBlockHash :: Http.T -> Int -> Int -> IO BlockHash
getSnapshotBlockHash connection bakingCycleNumber snapshotCycleNumber = do
  Tagged blockHashInBakingCycle <- getLastBlockHash connection bakingCycleNumber
  indices <-
    Http.direct_
      Nothing -- Disable timeout.
      (Http.request
         (Proxy :: Proxy GetBlockSnapshotIndices)
         mainChain
         blockHashInBakingCycle
         bakingCycleNumber)
      connection
  when (length indices /= 1) $ throw UnexpectedResultException
  snapshotIndex <- fromJustUnsafe UnexpectedResultException (head indices)
  -- ^ Baking cycle should only have one valid snapshot if it is complete.
  let snapshotBlockNumber =
        firstBlockNumberInCycle snapshotCycleNumber +
        snapshotOffset snapshotIndex
  getBlockHash connection snapshotBlockNumber

-- | Calculates reward info based on a delegate, the last block in a baking cycle, and the snapshot
-- block where baking rights were calculated for the baking cycle. This interface allows efficient
-- requests, even though the snapshot block is redundant information.
getRewardInfoSingle ::
     Http.T -> BlockHash -> BlockHash -> ImplicitAccount -> IO RewardInfo
getRewardInfoSingle connection (Tagged rewardBlockHash) (Tagged snapshotBlockHash) (Tagged delegateId) = do
  frozenBalanceCycles <-
    Http.direct
      (Http.request
         (Proxy :: Proxy ListFrozenBalanceCycles)
         mainChain
         rewardBlockHash
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
         snapshotBlockHash
         delegateId)
      connection
  stakingBalance <- toFixedQtyUnsafe (staking_balance delegateSnapshot)
  delegatedBalance <- toFixedQtyUnsafe (delegated_balance delegateSnapshot)
  let delegators = delegated_contracts delegateSnapshot
  delegations <-
    mapConcurrently
      (\delegator -> do
         size <-
           Http.direct
             (Http.request
                (Proxy :: Proxy GetContractBalance)
                mainChain
                snapshotBlockHash
                delegator)
             connection >>=
           toFixedQtyUnsafe
         return DelegationInfo {delegator = Tagged delegator, size})
      delegators
  return
    RewardInfo
      { delegate = Tagged delegateId
      , reward
      , stakingBalance
      , delegatedBalance
      , delegations
      }

getRewardInfo :: Http.T -> Int -> [ImplicitAccount] -> IO [RewardInfo]
getRewardInfo connection cycleNumber delegateIds = do
  let rewardBlockCycle = bakingCycle cycleNumber
      snapshotBlockCycle = snapshotCycle cycleNumber
  when (snapshotBlockCycle < 0) $ throw UnexpectedResultException
  rewardBlockHash <- getLastBlockHash connection rewardBlockCycle
  snapshotBlockHash <-
    getSnapshotBlockHash connection rewardBlockCycle snapshotBlockCycle
  mapM
    (getRewardInfoSingle connection rewardBlockHash snapshotBlockHash)
    delegateIds
