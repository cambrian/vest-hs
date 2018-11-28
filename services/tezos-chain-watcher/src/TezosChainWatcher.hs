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

-- | Eventually: Draw a nice state machine for ease of understanding.
monitorOperation ::
     T
  -> (Tezos.OperationHash, Word8)
  -> IO (Stream ValueBuffer Tezos.OperationStatus)
monitorOperation T {dbPool, provisionalBlockEventStream} (opHash, threshold) = do
  (statusWriter, statusStream) <- newStream
  lastBlockHashVar <- newEmptyTMVarIO
  opStatusVar <- newEmptyTMVarIO
  -- Save a new op status.
  let saveOpStatus newOpStatus = do
        void $ atomically $ swapTMVar opStatusVar newOpStatus
        writeStream statusWriter newOpStatus
  -- See if a block event contains the op hash being monitored.
  let opContainedIn Tezos.BlockEvent {operations} = opHash `elem` operations
  -- Query the DB for an op status.
  let materializeOpStatus currentBlockNumber = do
        blockInfoMaybe <- Pg.runLogged dbPool $ selectBlockInfoForOpHash opHash
        case blockInfoMaybe of
          Nothing -> return $ Tezos.NotIncluded 0
          Just (blockHash, blockNumber) -> do
            let confirmations = fromIntegral $ currentBlockNumber - blockNumber
            -- ^ If confirmations is negative, it means we had a forked chain whose head we are
            -- still catching up to. A block that is yet to be streamed contains our operation.
            if confirmations < 0
              then return $ Tezos.NotIncluded 0
              else if confirmations < threshold
                     then return $ Tezos.Included (blockHash, confirmations)
                     else return $ Tezos.Confirmed blockHash
  void . async $
    takeWhileMStream -- Continue until op confirmed or rejected.
      (\event@Tezos.BlockEvent {hash, number, predecessor} -> do
         lastBlockHashMaybe <- atomically $ tryReadTMVar lastBlockHashVar
         atomically $ writeTMVar lastBlockHashVar hash
         -- ^ Update last block hash for next iteration.
         case lastBlockHashMaybe of
           Nothing -> return True
           -- ^ Wait for one block to go by so we can ensure the validity of the DB query within
           -- block events we have seen. If the chain subsequently forks, we simply re-run the DB
           -- query to restore validity.
           Just lastBlockHash -> do
             opStatusMaybe <- atomically (tryReadTMVar opStatusVar)
             opStatus <-
               case opStatusMaybe of
                 Nothing -> materializeOpStatus number
                 Just _
                   | predecessor /= lastBlockHash -> materializeOpStatus number
                 -- ^ Cases for the start of monitoring and chain forking.
                 Just (Tezos.NotIncluded c) ->
                   if c < threshold
                     then return $
                          if opContainedIn event
                            then Tezos.Included (hash, 0)
                            else Tezos.NotIncluded (c + 1)
                     else return Tezos.Rejected
                 Just (Tezos.Included (b, c)) ->
                   if c < threshold
                     then return $ Tezos.Included (b, c + 1)
                     else return $ Tezos.Confirmed b
                 _ -> throw BugException
             -- ^ Generate a new op status.
             void $ saveOpStatus opStatus
             case opStatus of
               Tezos.Confirmed _ -> return False
               Tezos.Rejected -> return False
               _ -> return True)
      provisionalBlockEventStream
  return statusStream

-- | This DB materializer waits on persistence of the desired event to occur.
materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t@T {dbPool} blockNumber = do
  finalizedHeightStream <- subscribe t (Proxy :: Proxy FinalizedHeightValue)
  takeWhileMStream (return . (blockNumber >)) finalizedHeightStream >>=
    waitStream
  Pg.runLogged dbPool (selectFinalizedBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

streamBlockEvents ::
     Pool (Specific T Pg.Connection)
  -> Tezos.Rpc.T
  -> TezosFinalizationLag
  -> IO ( Stream QueueBuffer Tezos.BlockEvent
        , Stream QueueBuffer Tezos.BlockEvent
        , Stream ValueBuffer Word64)
streamBlockEvents dbPool tezos (TezosFinalizationLag finalizationLag) = do
  highestSeenBlockNumber <- Pg.runLogged dbPool (selectMaxBlockNumber Finalized)
  let nextBlockNumber = highestSeenBlockNumber + 1
  provisionalHeightVar <-
    Pg.runLogged dbPool (selectMaxBlockNumber NotDeleted) >>= newTVarIO
  (finalizedEventStream_, provisionalEventStream_, invalidatedHashesStream) <-
    Tezos.streamBlockEventsDurable tezos finalizationLag nextBlockNumber
  (finalizedHeightWriter, finalizedHeightStream) <- newStream
  -- Persist finalized block events before streaming them.
  finalizedEventStream <-
    mapMStream
      (\event -> do
         updatedAt <- now
         let deletedAt = updatedAt
             Tezos.BlockEvent {hash, number} = event
         provisionalHeight <- readTVarIO provisionalHeightVar
         log Debug "waiting to persist block event" (number, provisionalHeight)
         atomically $ readTVar provisionalHeightVar >>= check . (number <=)
         -- ^ Ensure that we have seen provisional events at at least this block level (we want
         -- finalized block events to strictly lag their provisional counterparts).
         log Debug "persisting block event" number
         Pg.runLoggedTransaction
           dbPool
           (do finalized <- finalizeProvisionalBlockEvent updatedAt hash
               unless finalized $ liftIO $ throw BugException
               deleteOldProvisionalBlockEvents deletedAt number)
         -- ^ Promote a provisional event to a finalized event (a provisional event must exist with
         -- the desired hash, or something is wrong with our DB state).
         writeStream finalizedHeightWriter number
         log Debug "persisted block event" number
         return event)
      finalizedEventStream_
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
         provisionalHeight <- readTVarIO provisionalHeightVar
         when (number > provisionalHeight) $
           atomically $ writeTVar provisionalHeightVar number
         log Debug "persisted provisional block event" (number, hash)
         return event)
      provisionalEventStream_
  -- Add no-op consumers to make sure finalized/provisional event writes do not block.
  mapM_
    (tapStream_ $ void . return)
    [finalizedEventStream, provisionalEventStream]
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
  return (finalizedEventStream, provisionalEventStream, finalizedHeightStream)

instance Service T where
  type RpcSpec T = RewardInfoEndpoint
                   :<|> MonitorOperationEndpoint
  type ValueSpec T = FinalizedHeightValue
  type EventsProduced T = FinalizedBlockEvents
  type EventsConsumed T = ()
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> finalizationLag) <- load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      (finalizedBlockEventStream, provisionalBlockEventStream, finalizedHeightStream) <-
        streamBlockEvents dbPool tezos finalizationLag
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , finalizedBlockEventStream
          , provisionalBlockEventStream
          , finalizedHeightStream
          }
  rpcHandlers t = rewardInfo t :<|> monitorOperation t
  valuesPublished = finalizedHeightStream
  eventProducers t =
    (return $ finalizedBlockEventStream t, materializeBlockEvent t)
  eventConsumers _ = ()
