module Tezos.Rpc.Internal
  ( module Tezos.Rpc.Internal
  ) where

import Control.Retry
import Data.Aeson
import qualified Http
import Tezos.Prelude
import qualified Tezos.Prelude as Prelude (BlockEvent(..))
import Tezos.Rpc.Prelude hiding
  ( Operation(..)
  , Origination(..)
  , Transaction(..)
  )
import qualified Tezos.Rpc.Prelude as Rpc
  ( Operation(..)
  , Origination(..)
  , Transaction(..)
  )
import Vest hiding (hash)

data UnexpectedResultException =
  UnexpectedResultException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

toFixedQtyUnsafe :: Text -> IO (FixedQty XTZ)
toFixedQtyUnsafe x = readUnsafe @Integer x >>- fromInteger

-- If Tezos ever hard-forks, our hard-coded constants might break. For now, they make this library
-- significantly easier to implement.
-- Eventually: Use ratios here?
blocksPerCycle :: Int
blocksPerCycle = 4096

blocksPerSnapshot :: Int
blocksPerSnapshot = 256

firstBlockNumber :: Int
firstBlockNumber = 1

firstReadableBlockNumber :: Int
firstReadableBlockNumber = 2

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

getBlock :: Http.Client -> Text -> IO Block
getBlock httpClient hash =
  Http.request httpClient $
  Http.buildRequest (Proxy :: Proxy GetBlock) mainChain hash

getHeadBlock :: Http.Client -> IO Block
getHeadBlock httpClient = getBlock httpClient headBlockHash

-- | Will throw if targetBlockNumber is in the future.
getBlockHash :: Http.Client -> Int -> IO Text
getBlockHash httpClient targetBlockNumber = do
  latestBlock <- getBlock httpClient headBlockHash
  -- Ugly let because of duplicate record fields.
  let Block {hash = latestBlockHash} = latestBlock
      BlockHeader {level = latestBlockNumber} = header latestBlock
      offset = latestBlockNumber - targetBlockNumber
  when (offset < 0) $ throw UnexpectedResultException
  Http.request httpClient $
    Http.buildRequest
      (Proxy :: Proxy GetBlockHash)
      mainChain
      (latestBlockHash <> "~" <> show offset)
  -- ^ We query a given block number using hash HASH~X, which represents the block X levels before
  -- the block represented by HASH.

getLastBlockHash :: Http.Client -> Int -> IO Text
getLastBlockHash httpClient cycleNumber =
  getBlockHash httpClient (lastBlockNumberInCycle cycleNumber)

-- | Takes a while to run, but we only have to run this once per cycle.
getSnapshotBlockHash :: Http.Client -> Int -> Int -> IO Text
getSnapshotBlockHash httpClient bakingCycleNumber snapshotCycleNumber = do
  blockHashInBakingCycle <- getLastBlockHash httpClient bakingCycleNumber
  indices <-
    Http.request httpClient $
    Http.buildRequest
      (Proxy :: Proxy GetBlockSnapshotIndices)
      mainChain
      blockHashInBakingCycle
      bakingCycleNumber
  when (length indices /= 1) $ throw UnexpectedResultException
  snapshotIndex <- fromJustUnsafe UnexpectedResultException (head indices)
  -- ^ Baking cycle should only have one valid snapshot if it is complete.
  let snapshotBlockNumber =
        firstBlockNumberInCycle snapshotCycleNumber +
        snapshotOffset snapshotIndex
  getBlockHash httpClient snapshotBlockNumber

-- | Calculates reward info based on a delegate, the last block in a baking cycle, and the snapshot
-- block where baking rights were calculated for the baking cycle. This interface allows efficient
-- requests, even though the snapshot block is redundant information.
--
-- Please refactor to take tezos types instead of Text.
-- You should give the other fns in this file a look over too.
getRewardInfoSingle :: Http.Client -> Text -> Text -> Text -> IO RewardInfo
getRewardInfoSingle httpClient rewardBlockHash snapshotBlockHash delegateId = do
  frozenBalanceCycles <-
    Http.request httpClient $
    Http.buildRequest
      (Proxy :: Proxy ListFrozenBalanceCycles)
      mainChain
      rewardBlockHash
      delegateId
  frozenBalance <-
    fromJustUnsafe UnexpectedResultException (last frozenBalanceCycles)
  reward <-
    (+) <$> toFixedQtyUnsafe (rewards frozenBalance) <*>
    toFixedQtyUnsafe (fees frozenBalance)
  delegateSnapshot <-
    Http.request httpClient $
    Http.buildRequest
      (Proxy :: Proxy GetDelegate)
      mainChain
      snapshotBlockHash
      delegateId
  stakingBalance <- toFixedQtyUnsafe $ staking_balance delegateSnapshot
  delegatedBalance <- toFixedQtyUnsafe $ delegated_balance delegateSnapshot
  let delegators = delegated_contracts delegateSnapshot
  delegations <-
    mapConcurrently
      (\delegatorRaw -> do
         size <-
           Http.request
             httpClient
             (Http.buildRequest
                (Proxy :: Proxy GetContractBalance)
                mainChain
                snapshotBlockHash
                delegatorRaw) >>=
           toFixedQtyUnsafe
         return DelegationInfo {delegator = Tagged $ Tagged delegatorRaw, size})
      delegators
  return
    RewardInfo
      { delegate = Tagged $ Tagged delegateId
      , reward
      , stakingBalance
      , delegatedBalance
      , delegations
      }

extractOriginations :: Text -> Rpc.Operation -> [Origination]
extractOriginations hash nodeOp =
  case nodeOp of
    Rpc.OriginationOp Rpc.Origination { source
                                      , metadata = OriginationMetadata {operation_result = OriginationResult {originated_contracts}}
                                      } ->
      fmap
        (\originated ->
           Origination
             { hash = Tagged hash
             , originator = Tagged $ Tagged source
             , originated = Tagged $ Tagged originated
             })
        (fromMaybe [] originated_contracts)
    _ -> []

extractTransaction :: Text -> Rpc.Operation -> IO (Maybe Transaction)
extractTransaction hash nodeOp =
  case nodeOp of
    Rpc.TransactionOp Rpc.Transaction { source
                                      , destination
                                      , fee = feeRaw
                                      , amount = sizeRaw
                                      } -> do
      fee <- toFixedQtyUnsafe feeRaw
      size <- toFixedQtyUnsafe sizeRaw
      return $
        Just $
        Transaction
          { hash = Tagged hash
          , from = Tagged source
          , to = Tagged destination
          , fee
          , size
          }
    _ -> return Nothing

toBlockEvent :: Block -> IO BlockEvent
toBlockEvent Block {hash, header, metadata, operations = operationsRaw} = do
  let BlockHeader {level = number, predecessor, timestamp} = header
      BlockMetadata {level = LevelInfo {cycle = cycleNumber}} = metadata
      operationGroups = concat operationsRaw
      operations = fmap (\OperationGroup {hash} -> Tagged hash) operationGroups
      originations =
        concatMap
          (\OperationGroup {hash, contents = operations} ->
             concatMap (extractOriginations hash) operations)
          operationGroups
  transactions <-
    concatMapM
      (\OperationGroup {hash, contents = operations} ->
         mapMaybeM (extractTransaction hash) operations)
      operationGroups
  return
    BlockEvent
      { number = fromIntegral number -- Int to Word64.
      , hash = Tagged hash
      , predecessor = Tagged predecessor
      , cycleNumber = fromIntegral cycleNumber
      , fee = opFee
      , time = timestamp
      , operations
      , originations
      , transactions
      }

recoveryCases :: [RetryStatus -> Handler IO Bool]
recoveryCases =
  [ logRetries
      (\(_ :: Http.ServantError) -> return True)
      (\b e r -> log Debug "Servant error" $ take 35 $ defaultLogMsg b e r)
  , logRetries
      (\(_ :: UnexpectedResultException) -> return True)
      (\b e r -> log Debug "unexpected result" $ take 38 $ defaultLogMsg b e r)
  ]

materializeBlockEvent :: Http.Client -> Int -> IO BlockEvent
materializeBlockEvent httpClient blockNumber = do
  blockHash <- getBlockHash httpClient blockNumber
  block <- getBlock httpClient blockHash
  let Block {header = BlockHeader {level}} = block
  when (level /= blockNumber) $ throw UnexpectedResultException
  -- ^ Sometimes the genesis block 0 randomly pops out of a query...
  toBlockEvent block

-- | Please see streamNewBlockEventsDurable to understand this fn.
-- TODO: Make this return Either to avoid gratuitous IO?
updateEventQueue ::
     Int -> [BlockEvent] -> BlockEvent -> IO ([BlockEvent], Maybe BlockEvent)
updateEventQueue finalizationLag queue newEvent = do
  let BlockEvent {predecessor} = newEvent
  augmentedQueue <-
    case head queue of
      Nothing -> return [newEvent]
      Just _ -> do
        let dropQueue =
              dropWhile (\BlockEvent {hash} -> predecessor /= hash) queue
        when (null dropQueue) $ throw UnexpectedResultException
        return $ newEvent : dropQueue
  if length augmentedQueue > finalizationLag
    then do
      newQueue <- fromJustUnsafe BugException $ initMay augmentedQueue
      lastItem <- Just <$> fromJustUnsafe BugException (last augmentedQueue)
      return (newQueue, lastItem)
    else return (augmentedQueue, Nothing)

materializeRetryPolicy :: RetryPolicy
materializeRetryPolicy = exponentialBackoff $ 2 * 1000000

blockRetryPolicy :: RetryPolicy
blockRetryPolicy = constantDelay $ 60 * 1000000
