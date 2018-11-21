module Tezos.Rpc
  ( module Reexports
  , T(..)
  , getRewardInfo
  , streamBlockEventsDurable
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
    (getRewardInfoSingle httpClient rewardBlockHash snapshotBlockHash .
     untag . untag)
    delegateIds

-- | This function polls every minute for the next sequential block and auto-retries when HTTP
-- requests fail. (It is worth noting that Tezos has its own monitoring API, but it is based on
-- chunked HTTP responses and is not very reliable in prod.)
--
-- This function also handles, within some notion of finality, the possibility of a chain forking.
--
-- Consider a queue of block hashes E, D, C, B, A and a desired finalization lag of 5. When the next
-- block F comes in, we will normally stream block A and edit the queue to be F, E, D, C, B.
--
-- If the next block we see is F, but it claims its predecessor is some block X, we will rewind over
-- predecessors until we see a block in our queue (B for instance). We edit the queue accordingly to
-- reflect the re-organization, so it might look like F, X, Y, B, A.
--
-- Note that the next block to write (A) has not reached 5 confirmations after the chain re-org, so
-- we write no events out.
--
-- Note also that Blocks E, D, and C are now invalidated, so we stream them in a separate variable.
--
-- If the predecessors of F do not terminate in our block queue within the 5 block lag, a larger
-- re-organization than we can handle has occurred. We can only error in this instance and fix the
-- problem manually.
streamBlockEventsDurable ::
     T
  -> Int
  -> IndexOf BlockEvent
  -> IO (Stream QueueBuffer BlockEvent, Stream QueueBuffer [BlockHash]) -- Events/invalidations.
streamBlockEventsDurable T {httpClient} finalizationLag startBlockNumber = do
  let updateEventQueue = updateEventQueueWith httpClient finalizationLag
  (finalWriter, finalStream) <- newStream
  (invalidationWriter, invalidationStream) <- newStream
  let streamQueuedFrom eventQueue blockNumber = do
        provisionalEvent <-
          recovering
            blockRetryPolicy
            recoveryCases
            (const $ materializeBlockEvent httpClient (fromIntegral blockNumber))
        let BlockEvent {number = provisionalNumber} = provisionalEvent
        (newEventQueue, invalidationsOrEvents) <-
          updateEventQueue eventQueue provisionalEvent
          -- ^ Recovers and retries internally if its HTTP requests fail.
        when (finalizationLag > 0) $
          log Debug "queued provisional block event" provisionalNumber
        case invalidationsOrEvents of
          Left invalidations -> do
            writeStream invalidationWriter invalidations
            log Debug "pushed block invalidations" invalidations
          Right finalEvents ->
            mapM_
              (\finalEvent -> do
                 writeStream finalWriter finalEvent
                 let BlockEvent {number = finalNumber} = finalEvent
                 let numberAndLag = (finalNumber, finalizationLag)
                 log Debug "produced block event on lag" numberAndLag)
              finalEvents
        streamQueuedFrom newEventQueue (blockNumber + 1)
  async $ streamQueuedFrom [] startBlockNumber
  return (finalStream, invalidationStream)

toCycleEventStream ::
     Stream QueueBuffer BlockEvent
  -> IndexOf CycleEvent
  -> IO (Stream QueueBuffer CycleEvent)
toCycleEventStream blockEventStream startLastSeen = do
  lastSeenVar <- newTVarIO startLastSeen
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
