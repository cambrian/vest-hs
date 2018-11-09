module Tezos.Node
  ( module Reexports
  , getRewardInfo
  , materializeBlockEventDurable
  , streamNewBlockEventsDurable
  , toCycleEventStream
  ) where

import Control.Retry
import qualified Http
import Tezos.Node.Internal
import Tezos.Node.Internal as Reexports (blocksPerCycle)
import Tezos.Node.Prelude
import Tezos.Prelude
import Vest hiding (hash)

getRewardInfo ::
     Http.T -> IndexOf CycleEvent -> [ImplicitAddress] -> IO [RewardInfo]
getRewardInfo connection cycleNumber delegateIds = do
  let rewardBlockCycle = bakingCycle (fromIntegral cycleNumber)
      snapshotBlockCycle = snapshotCycle (fromIntegral cycleNumber)
  when (snapshotBlockCycle < 0) $ throw UnexpectedResultException
  rewardBlockHash <- getLastBlockHash connection rewardBlockCycle
  snapshotBlockHash <-
    getSnapshotBlockHash connection rewardBlockCycle snapshotBlockCycle
  mapM
    (getRewardInfoSingle connection rewardBlockHash snapshotBlockHash . untag)
    delegateIds

materializeBlockEventDurable ::
     Http.T -> Logger -> IndexOf BlockEvent -> IO BlockEvent
materializeBlockEventDurable connection logger blockNumber =
  recovering
    materializeRetryPolicy -- Configurable retry limit?
    (recoveryCases logger)
    (const $
     log_ logger Debug ("Materializing block " <> show blockNumber <> ".") >>
     materializeBlockEvent_ connection (fromIntegral blockNumber))

-- | Recovering when a chunked stream fails is messier than just polling every minute for a new
-- block, so we opt for the latter solution when streaming block events.
streamNewBlockEventsDurable ::
     Http.T -> Logger -> IO (Stream QueueBuffer BlockEvent)
streamNewBlockEventsDurable connection logger = do
  let streamFrom blockNumber writer = do
        recovering
          blockRetryPolicy -- Configurable retry limit?
          (recoveryCases logger)
          (const $
           log_ logger Debug ("Streaming block " <> show blockNumber <> ".") >>
           materializeBlockEvent_ connection (fromIntegral blockNumber) >>=
           writeStream writer)
        -- ^ Eagerly runs (and probably fails the first time) each block, which is useful if we've
        -- gotten behind due to a network outage. This works, but could be improved.
        streamFrom (blockNumber + 1) writer
  LevelInfo {level = startBlockNumber} <-
    Http.direct
      (Http.request (Proxy :: Proxy GetBlockLevel) mainChain headBlockHash)
      connection
  (writer, stream) <- newStream
  async $ streamFrom startBlockNumber writer
  return stream

toCycleEventStream ::
     Stream QueueBuffer BlockEvent
  -> IndexOf CycleEvent
  -> IO (Stream QueueBuffer CycleEvent)
toCycleEventStream blockEventStream lastSeen = do
  lastSeenVar <- newTVarIO lastSeen
  filterMapMStream
    (\BlockEvent {cycleNumber, time} -> do
       let lastCycleNumber = cycleNumber - 1
       lastSeen <- readTVarIO lastSeenVar
       if lastCycleNumber <= lastSeen
         then return Nothing
         else do
           when (lastCycleNumber /= lastSeen + 1) $
             throw UnexpectedResultException
           atomically $ swapTVar lastSeenVar lastCycleNumber
           return $ Just $ CycleEvent {number = lastCycleNumber, time})
    blockEventStream
