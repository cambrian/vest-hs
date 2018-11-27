-- TODO: Add more logging.
module TezosChainWatcher
  ( module TezosChainWatcher
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import qualified Tezos.Rpc
import qualified Tezos.Rpc as Tezos
import TezosChainWatcher.Api as TezosChainWatcher
import TezosChainWatcher.Db
import TezosChainWatcher.Internal as TezosChainWatcher
import Vest hiding (hash)

rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

-- | This DB materializer waits on persistence of the desired event to occur.
materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t@T {dbPool} blockNumber = do
  highestSeenStream <- subscribe t (Proxy :: Proxy HighestSeenBlockNumberValue)
  takeWhileMStream (return . (blockNumber >)) highestSeenStream >>= waitStream
  Pg.runLogged dbPool (selectFinalBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

streamBlockEvents ::
     Pool (Specific T Pg.Connection)
  -> Tezos.Rpc.T
  -> TezosFinalizationLag
  -> IO ( Stream QueueBuffer Tezos.BlockEvent
        , Stream QueueBuffer Tezos.BlockEvent
        , Stream ValueBuffer Word64)
streamBlockEvents dbPool tezos (TezosFinalizationLag finalizationLag) = do
  highestSeenBlockNumber <- Pg.runLogged dbPool (selectMaxBlockNumber Final)
  let nextBlockNumber = highestSeenBlockNumber + 1
  highestSeenProvisionalBlockNumberVar <-
    Pg.runLogged dbPool (selectMaxBlockNumber NotDeleted) >>= newTVarIO
  (finalEventStream_, provisionalEventStream_, invalidatedHashesStream) <-
    Tezos.streamBlockEventsDurable tezos finalizationLag nextBlockNumber
  (highestSeenWriter, highestSeenStream) <- newStream
  -- Persist final block events before streaming them.
  finalEventStream <-
    mapMStream
      (\event -> do
         updatedAt <- now
         let deletedAt = updatedAt
             Tezos.BlockEvent {hash, number} = event
         highestSeen <- readTVarIO highestSeenProvisionalBlockNumberVar
         log Debug "waiting to persist block event" (number, highestSeen)
         atomically $
           readTVar highestSeenProvisionalBlockNumberVar >>= check . (number <=)
         -- ^ Ensure that we have seen provisional events at at least this block level (we want
         -- final block events to strictly lag their provisional counterparts).
         log Debug "persisting block event" number
         Pg.runLoggedTransaction
           dbPool
           (do finalized <- finalizeProvisionalBlockEvent updatedAt hash
               unless finalized $ liftIO $ throw BugException
               deleteOldProvisionalBlockEvents deletedAt number)
         -- ^ Promote a provisional event to a final event (a provisional event must exist with the
         -- desired hash, or something is wrong with our DB state).
         writeStream highestSeenWriter number
         log Debug "persisted block event" number
         return event)
      finalEventStream_
  -- Persist provisional block events before streaming them.
  provisionalEventStream <-
    mapMStream
      (\event -> do
         createdAt <- now
         let Tezos.BlockEvent {hash, number} = event
         log Debug "persisting provisional block event" (number, hash)
         Pg.runLoggedTransaction
           dbPool
           (insertProvisionalBlockEvent createdAt event)
         highestSeen <- readTVarIO highestSeenProvisionalBlockNumberVar
         when (number > highestSeen) $
           atomically $
           writeTVar highestSeenProvisionalBlockNumberVar highestSeen
         log Debug "persisted provisional block event" (number, hash)
         return event)
      provisionalEventStream_
  -- Invalidate provisional events as necessary.
  tapStream_
    (\hashes -> do
       deletedAt <- now
       log Debug "invalidating provisional block events" hashes
       Pg.runLoggedTransaction
         dbPool
         (deleteProvisionalBlockEventsByHash deletedAt hashes)
       log Debug "invalidated provisional block events" hashes)
    invalidatedHashesStream
  return (finalEventStream, provisionalEventStream, highestSeenStream)

instance Service T where
  type RpcSpec T = RewardInfoEndpoint
  type ValueSpec T = HighestSeenBlockNumberValue
  type EventsProduced T = BlockEvents
  type EventsConsumed T = ()
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> finalizationLag) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      (finalEventStream, provisionalEventStream, highestSeenStream) <-
        streamBlockEvents dbPool tezos finalizationLag
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , finalEventStream
          , provisionalEventStream
          , highestSeenStream
          }
  rpcHandlers = rewardInfo
  valuesPublished = highestSeenStream
  eventProducers t = (return $ finalEventStream t, materializeBlockEvent t)
  eventConsumers _ = ()
