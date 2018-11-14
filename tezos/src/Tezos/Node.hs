module Tezos.Node
  ( module Reexports
  , T(..)
  , getRewardInfo
  , materializeBlockEventDurable
  , streamNewBlockEventsDurable
  , toCycleEventStream
  ) where

import Control.Retry
import qualified Http
import Tezos.Node.Internal
import Tezos.Node.Internal as Reexports
  ( blocksPerCycle
  , firstReadableBlockNumber
  )
import Tezos.Node.Prelude
import Tezos.Prelude
import Vest hiding (hash)

data T = T
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
    (const $ do
       log Debug "materializing block event" blockNumber
       event <- materializeBlockEvent_ httpClient (fromIntegral blockNumber)
       log Debug "materialized block event" blockNumber >> return event)

-- | Recovering when a chunked stream fails is messier than just polling every minute for a new
-- block, so we opt for the latter solution when streaming block events.
streamNewBlockEventsDurable :: T -> IO (Stream QueueBuffer BlockEvent)
streamNewBlockEventsDurable T {httpClient} = do
  let streamFrom blockNumber writer = do
        recovering
          blockRetryPolicy -- Configurable retry limit?
          recoveryCases
          (const $ do
             log Debug "producing block event" blockNumber
             event <-
               materializeBlockEvent_ httpClient (fromIntegral blockNumber)
             writeStream writer event
             log Debug "produced block event" blockNumber)
        -- ^ Eagerly runs (and probably fails the first time) each block, which is useful if we've
        -- gotten behind due to a network outage. This works, but could be improved.
        streamFrom (blockNumber + 1) writer
  LevelInfo {level = startBlockNumber} <-
    Http.request httpClient $
    Http.buildRequest (Proxy :: Proxy GetBlockLevel) mainChain headBlockHash
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
