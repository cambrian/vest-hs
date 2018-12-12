module Tezos.Rpc
  ( module Reexports
  , T(..)
  , getCounter
  , getRewardInfo
  , streamProvisionalBlockEvents
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

getCounter :: T -> Address -> IO Word64
getCounter T {httpClient} (Tagged address) =
  fromIntegral <$> getCounterRaw httpClient address

getRewardInfo ::
     T
  -> Word64
  -> [ImplicitAddress]
  -> IO (Either InvalidCycleException [RewardInfo])
getRewardInfo T {httpClient} rewardBlockCycle_ delegateIds = do
  let rewardBlockCycle = fromIntegral rewardBlockCycle_
      snapshotBlockCycle = snapshotCycle rewardBlockCycle
  -- Reward info fails for cycles in the future, cycles where any delegate did not bake, and cycles
  -- that are currently incomplete.
  if snapshotBlockCycle < 0
    then return $ Right []
    else catch
           (do rewardBlockHash <- getLastBlockHash httpClient rewardBlockCycle
               snapshotBlockHash <-
                 getSnapshotBlockHash
                   httpClient
                   rewardBlockCycle
                   snapshotBlockCycle
               Right <$>
                 mapM
                   (getRewardInfoSingle
                      httpClient
                      rewardBlockHash
                      snapshotBlockHash .
                    untag . untag)
                   delegateIds)
           (\(_ :: UnexpectedResultException) ->
              return $ Left InvalidCycleException)

-- | This function polls every minute for the next sequential block and auto-retries when HTTP
-- requests fail. (It is worth noting that Tezos has its own monitoring API, but it is based on
-- chunked HTTP responses and is not very reliable in prod.)
--
-- This function also handles, within some notion of finality, the possibility of a chain forking.
--
-- Consider a queue of block hashes E, D, C, B, A and a desired finalization lag of 5. When the next
-- block F comes in, we will normally finalize block A and edit the queue to be F, E, D, C, B.
--
-- If the next block we see is F, but it claims its predecessor is some block X, we will rewind over
-- predecessors until we see a block in our queue (B for instance). We edit the queue accordingly to
-- reflect the re-organization, so it might look like F, X, Y, B, A.
--
-- Note that the next block to write (A) has not reached 5 confirmations after the chain re-org, so
-- we finalize no events.
--
-- Note also that Blocks E, D, and C are now invalidated, so we stream them in a separate variable.
--
-- If the predecessors of F do not terminate in our block queue within the 5 block lag, a larger
-- re-organization than we can handle has occurred. We can only error in this instance and fix the
-- problem manually.
streamProvisionalBlockEvents ::
     T
  -> Word64
  -> IndexOf BlockEvent
  -> IO (Stream QueueBuffer BlockEvent, Stream QueueBuffer [BlockHash])
streamProvisionalBlockEvents T {httpClient} finalizationLag startBlockNumber = do
  let updateEventQueue = updateEventQueueWith httpClient finalizationLag
  (provisionalEventWriter, provisionalEventStream) <- newStream
  (invalidatedHashesWriter, invalidatedHashesStream) <- newStream
  let streamQueuedFrom eventQueue blockNumber = do
        newEvent <-
          recovering
            requestRetryPolicy
            recoveryCases
            (const $ materializeBlockEvent httpClient (fromIntegral blockNumber))
        -- ^ TODO: Modify retry between the materializing and streaming states.
        (newEventQueue, newProvisionalEvents, invalidatedProvisionalHashes) <-
          updateEventQueue eventQueue newEvent
        -- ^ Recovers and retries internally if its HTTP requests fail.
        mapM_
          (\event -> do
             writeStream provisionalEventWriter event
             let BlockEvent {number, hash} = event
             log Debug "pushed provisional block event" (number, hash))
          newProvisionalEvents
        unless (null invalidatedProvisionalHashes) $ do
          writeStream invalidatedHashesWriter invalidatedProvisionalHashes
          log
            Debug
            "pushed invalidated block hashes"
            invalidatedProvisionalHashes
        streamQueuedFrom newEventQueue (blockNumber + 1)
  async $ streamQueuedFrom [] startBlockNumber
  return (provisionalEventStream, invalidatedHashesStream)
