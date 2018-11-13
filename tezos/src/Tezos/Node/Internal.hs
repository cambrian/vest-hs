module Tezos.Node.Internal
  ( module Tezos.Node.Internal
  ) where

import Control.Retry
import Data.Aeson
import qualified Http
import Tezos.Node.Prelude hiding
  ( Operation(..)
  , Origination(..)
  , Transaction(..)
  )
import qualified Tezos.Node.Prelude as Node
  ( Operation(..)
  , Origination(..)
  , Transaction(..)
  )
import Tezos.Prelude
import Vest hiding (hash)

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

opFee :: FixedQty "XTZ"
opFee = 0

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

getBlock :: Http.T -> Text -> IO Block
getBlock connection hash =
  Http.direct (Http.request (Proxy :: Proxy GetBlock) mainChain hash) connection

-- | Will throw if targetBlockNumber is in the future.
getBlockHash :: Http.T -> Int -> IO Text
getBlockHash connection targetBlockNumber = do
  latestBlock <- getBlock connection headBlockHash
  -- Ugly let because of duplicate record fields.
  let Block {hash = latestBlockHash} = latestBlock
      BlockHeader {level = latestBlockNumber} = header latestBlock
      offset = latestBlockNumber - targetBlockNumber
  when (offset < 0) $ throw UnexpectedResultException
  Http.direct
    (Http.request
       (Proxy :: Proxy GetBlockHash)
       mainChain
       (latestBlockHash <> "~" <> show offset))
    connection
  -- ^ We query a given block number using hash HASH~X, which represents the block X levels before
  -- the block represented by HASH.

getLastBlockHash :: Http.T -> Int -> IO Text
getLastBlockHash connection cycleNumber =
  getBlockHash connection (lastBlockNumberInCycle cycleNumber)

-- | Takes a while to run, but we only have to run this once per cycle.
getSnapshotBlockHash :: Http.T -> Int -> Int -> IO Text
getSnapshotBlockHash connection bakingCycleNumber snapshotCycleNumber = do
  blockHashInBakingCycle <- getLastBlockHash connection bakingCycleNumber
  indices <-
    Http.direct
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
getRewardInfoSingle :: Http.T -> Text -> Text -> Text -> IO RewardInfo
getRewardInfoSingle connection rewardBlockHash snapshotBlockHash delegateId = do
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

-- Note that several operations might share a batch hash. Moreover, a single Node operation can
-- sometimes represent multiple logical operations (e.g. when an origination creates multiple
-- contracts). I'm still not 100% sure when this can happen, but the API suggests the possibility.
toOperation :: Text -> Node.Operation -> IO [Operation]
toOperation hash nodeOp =
  case nodeOp of
    Node.OriginationOp Node.Origination { source = originator
                                        , metadata = OriginationMetadata {operation_result = OriginationResult {originated_contracts = contracts}}
                                        } ->
      return $
      fmap
        (\originated ->
           OriginationOp $
           Origination
             { hash = Tagged hash
             , originator = Tagged originator
             , originated = Tagged originated
             })
        (fromMaybe [] contracts)
    Node.TransactionOp Node.Transaction { source = from
                                        , destination = to
                                        , fee = feeRaw
                                        , amount = sizeRaw
                                        } -> do
      fee <- toFixedQtyUnsafe feeRaw
      size <- toFixedQtyUnsafe sizeRaw
      return
        [ TransactionOp $
          Transaction
            {hash = Tagged hash, from = Tagged from, to = Tagged to, fee, size}
        ]
    _ -> return [Other $ Tagged hash]

toBlockEvent :: Block -> IO BlockEvent
toBlockEvent Block {hash, header, metadata, operations = operationsRaw} = do
  let BlockHeader {level = number, time} = header
      BlockMetadata {level = LevelInfo {cycle = cycleNumber}} = metadata
  operations <-
    concatMapM
      (\OperationGroup {hash, contents} ->
         concatMapM (toOperation hash) contents)
      (concat operationsRaw)
  return
    BlockEvent
      { number = fromIntegral number -- Int to Word64.
      , hash = Tagged hash
      , cycleNumber = fromIntegral cycleNumber
      , fee = opFee
      , time
      , operations
      }

recoveryCases :: [RetryStatus -> Handler IO Bool]
recoveryCases =
  [ logRetries
      (\(_ :: Http.ServantError) -> return True)
      (\b e r -> log Debug $ pack $ defaultLogMsg b e r)
  , logRetries
      (\(_ :: UnexpectedResultException) -> return True)
      (\b e r -> log Debug $ pack $ defaultLogMsg b e r)
  ]

materializeBlockEvent_ :: Http.T -> Int -> IO BlockEvent
materializeBlockEvent_ connection blockNumber = do
  blockHash <- getBlockHash connection blockNumber
  getBlock connection blockHash >>= toBlockEvent

materializeRetryPolicy :: RetryPolicy
materializeRetryPolicy = exponentialBackoff $ 2 * 1000000

blockRetryPolicy :: RetryPolicy
blockRetryPolicy = constantDelay $ 60 * 1000000
