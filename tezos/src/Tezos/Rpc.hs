module Tezos.Rpc
  ( module Reexports
  , T(..)
  , getRewardInfo
  , materializeBlockEventDurable
  , streamNewBlockEventsDurable
  , toCycleEventStream
  ) where

import Control.Retry
import qualified Http
import Tezos.Prelude
import Tezos.Rpc.Internal
import Tezos.Rpc.Internal as Reexports
  ( blocksPerCycle
  , firstReadableBlockNumber
  )
import Tezos.Rpc.Prelude
import Vest hiding (hash)

newtype T = T
  { httpClient :: Http.Client
  }

instance Resource T where
  type ResourceConfig T = Http.Config
  make :: Http.Config -> IO T
  make config = do
    httpClient <- makeLogged @Http.Client config
    return $ T {httpClient}
  cleanup :: T -> IO ()
  cleanup = cleanupLogged . httpClient

instance Loadable T where
  configFile = [relfile|tezos-rpc.yaml|]

getRewardInfo :: T -> IndexOf CycleEvent -> [ImplicitAddress] -> IO [RewardInfo]
getRewardInfo T {httpClient} cycleNumber delegateIds = do
  let rewardBlockCycle = bakingCycle (fromIntegral cycleNumber)
      snapshotBlockCycle = snapshotCycle (fromIntegral cycleNumber)
  when (snapshotBlockCycle < 0) $ throw UnexpectedResultException
  rewardBlockHash <- getLastBlockHash httpClient rewardBlockCycle
  snapshotBlockHash <-
    getSnapshotBlockHash httpClient rewardBlockCycle snapshotBlockCycle
  mapM
    (getRewardInfoSingle httpClient rewardBlockHash snapshotBlockHash . untag)
    delegateIds

materializeBlockEventDurable :: T -> IndexOf BlockEvent -> IO BlockEvent
materializeBlockEventDurable T {httpClient} blockNumber =
  recovering
    materializeRetryPolicy -- Configurable retry limit?
    recoveryCases
    (const $ materializeBlockEvent_ httpClient (fromIntegral blockNumber))
    -- ^ Consider adding logging back here. It got cluttery...

-- | This function polls every minute for the next sequential block (Tezos also has a monitoring API
-- based on chunked HTTP responses, but it is not very reliable in prod). Auto-retry is built in
-- when HTTP requests fail.
--
-- This function also handles, within some notion of finality, the possibility of a chain forking.
--
-- Consider a queue of block hashes E, D, C, B, A and a desired finalization lag of 5. When the next
-- block F comes in, we will stream block A and edit the queue to be F, E, D, C, B.
--
-- If the next block we see is F, but it claims its predecessor is C, we will write nothing and edit
-- the queue to look like F, C, B, A. More specifically, the next block to write (A) has not reached
-- 5 confirmations after the chain re-org.
--
-- If F points to a predecessor not in the queue, we can only error and fix it manually.
streamNewBlockEventsDurable :: T -> Int -> IO (Stream QueueBuffer BlockEvent)
streamNewBlockEventsDurable T {httpClient} finalizationLag = do
  let streamQueuedFrom blockQueue blockNumber writer = do
        event <-
          recovering
            blockRetryPolicy -- Configurable retry limit?
            recoveryCases
            (const $
             materializeBlockEvent_ httpClient (fromIntegral blockNumber))
        -- ^ Eagerly runs (and probably fails the first time) each block, which is useful if we've
        -- gotten behind due to a network outage. This works, but could be improved.
        log Debug "queueing block event on lag" (blockNumber, finalizationLag)
        (newBlockQueue, nextEventMaybe) <-
          updateBlockQueue finalizationLag blockQueue event
        log Debug "queued block event on lag" (blockNumber, finalizationLag)
        case nextEventMaybe of
          Nothing -> return ()
          Just nextEvent -> do
            let BlockEvent {number} = nextEvent
            log Debug "producing block event on lag" (number, finalizationLag)
            writeStream writer nextEvent
            log Debug "produced block event on lag" (number, finalizationLag)
        streamQueuedFrom newBlockQueue (blockNumber + 1) writer
  LevelInfo {level = headBlockNumber} <-
    Http.request httpClient $
    Http.buildRequest (Proxy :: Proxy GetBlockLevel) mainChain headBlockHash
  (writer, stream) <- newStream
  let startBlockNumber =
        max (headBlockNumber - finalizationLag) firstReadableBlockNumber
  async $ streamQueuedFrom [] startBlockNumber writer
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
