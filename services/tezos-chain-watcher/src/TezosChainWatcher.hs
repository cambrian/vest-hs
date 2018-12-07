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

rewardInfo ::
     T
  -> RewardInfoRequest
  -> IO (Either Tezos.InvalidCycleException [Tezos.RewardInfo])
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

-- | Eventually: Draw a nice state machine for ease of understanding.
-- Review: the code for this service should be reworked. Summary of changes to be made:
-- - The provisional block height should just be a mapStream of the provisional block events.
-- - Monitor operation can be simplified by use of a fold instead of T(M)Vars
-- - Completing these changes will allow you to remove several mostly-redundant fields on the type
--   T.
monitorOperation ::
     T -> Tezos.OperationHash -> IO (Stream ValueBuffer Tezos.OperationStatus)
monitorOperation T {dbPool, provisionalBlockEventStream} opHash = do
  let threshold = Tezos.defaultFinalizationLag
  (statusWriter, statusStream) <- newStream
  lastBlockHashVar <- newEmptyTMVarIO
  opStatusVar <- newEmptyTMVarIO
  -- Save a new op status.
  let saveOpStatus newOpStatus = do
        void $ atomically $ writeTMVar opStatusVar newOpStatus
        writeStream statusWriter newOpStatus
  -- See if a block event contains the op hash being monitored.
  let opContainedIn Tezos.BlockEvent {operations} = opHash `elem` operations
  -- Query the DB for an op status.
  let materializeOpStatus currentBlockNumber = do
        blockInfoMaybe <- Pg.runLogged dbPool $ selectBlockInfoForOpHash opHash
        case blockInfoMaybe of
          Nothing -> return $ Tezos.NotIncluded 0
          Just (blockHash, blockNumber) -> do
            let (confirmations :: Int) =
                  fromIntegral currentBlockNumber - fromIntegral blockNumber
            -- ^ If confirmations is negative, it means we had a forked chain whose head we are
            -- still catching up to. A block that is yet to be streamed contains our operation.
            if confirmations < 0
              then return $ Tezos.NotIncluded 0
              else if confirmations < fromIntegral threshold
                     then return $
                          Tezos.Included (blockHash, fromIntegral confirmations)
                     else return $ Tezos.Confirmed blockHash
  async $
    consumeStream (void . return) =<<
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
                   return $
                   if c < threshold
                     then if opContainedIn event
                            then Tezos.Included (hash, 0)
                            else Tezos.NotIncluded (c + 1)
                     else Tezos.Rejected
                 Just (Tezos.Included (b, c)) ->
                   return $
                   if c < threshold
                     then Tezos.Included (b, c + 1)
                     else Tezos.Confirmed b
                 _ -> throw BugException
             -- ^ Generate a new op status.
             void $ saveOpStatus opStatus
             case opStatus of
               Tezos.Confirmed _ -> closeStream statusWriter >> return False
               Tezos.Rejected -> closeStream statusWriter >> return False
               _ -> return True)
      provisionalBlockEventStream
  return statusStream

-- | This DB materializer waits on persistence of the desired event to occur.
materializeFinalizedBlockEvent ::
     T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeFinalizedBlockEvent T {dbPool, finalizedHeightStream} blockNumber = do
  takeWhileMStream (return . (blockNumber >)) finalizedHeightStream >>=
    waitStream
  Pg.runLogged dbPool (selectFinalizedBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

-- Run on EVERY service replica.
streamProvisionalBlockEvents ::
     Pool (Specific T Pg.Connection)
  -> Tezos.Rpc.T
  -> Word8
  -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamProvisionalBlockEvents dbPool tezos finalizationLag = do
  highestSeenBlockNumber <- Pg.runLogged dbPool (selectMaxBlockNumber Finalized)
  let nextBlockNumber = highestSeenBlockNumber + 1
  (provisionalEventStream_, invalidatedHashesStream) <-
    Tezos.streamProvisionalBlockEvents tezos finalizationLag nextBlockNumber
  -- Persist provisional block events before streaming them.
  provisionalEventStream <-
    mapMStream
      (\event -> do
         createdAt <- now
         let Tezos.BlockEvent {hash, number} = event
         log Debug "persisting provisional block event" (number, hash)
         Pg.runLoggedTransaction
           dbPool
           (insertProvisionalBlockEventTx createdAt event)
         log Debug "persisted provisional block event" (number, hash)
         return event)
      provisionalEventStream_
  -- No-op consumer to make sure event writes do not block.
  tapStream_ (void . return) provisionalEventStream
  -- Invalidate provisional events as necessary.
  tapStream_
    (\hashes -> do
       deletedAt <- now
       log Debug "invalidating provisional block events" hashes
       Pg.runLoggedTransaction
         dbPool
         (deleteProvisionalBlockEventsTx deletedAt hashes)
       log Debug "invalidated provisional block events" hashes)
    invalidatedHashesStream
  return provisionalEventStream

streamProvisionalHeight :: T -> IO (Stream ValueBuffer Word64)
streamProvisionalHeight T {provisionalBlockEventStream} = do
  (provisionalHeightWriter, provisionalHeightStream) <- newStream
  async $
    foldMStream
      (\Tezos.BlockEvent {number} height ->
         if number > height
           then do
             writeStream provisionalHeightWriter number
             return number
           else return height)
      (fromIntegral $ min (Tezos.firstReadableBlockNumber - 1) 0)
      provisionalBlockEventStream
  return provisionalHeightStream

streamFinalizedBlockEvents ::
     T -> Stream ValueBuffer Word64 -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamFinalizedBlockEvents T {dbPool} provisionalHeightStream = do
  (finalizedEventWriter, finalizedEventStream) <- newStream
  tapStream_
    (\height -> do
       updatedAt <- now
       let maxBlockNumberToFinalize =
             height - fromIntegral Tezos.defaultFinalizationLag
       log Debug "finalizing to block level" maxBlockNumberToFinalize
       events <-
         Pg.runLoggedTransaction dbPool $
         finalizeProvisionalBlockEventsTx updatedAt maxBlockNumberToFinalize
       log Debug "finalized to block level" maxBlockNumberToFinalize
       mapM_
         (\event@Tezos.BlockEvent {number, hash} -> do
            log Debug "streaming block event" (number, hash)
            writeStream finalizedEventWriter event)
         events)
    provisionalHeightStream
  return finalizedEventStream

instance Service T where
  type RpcSpec T = RewardInfoEndpoint
                   :<|> MonitorOperationEndpoint
  type ValueSpec T = ProvisionalHeightValue
                     :<|> FinalizedHeightValue
                     :<|> OperationFeeValue
  type EventsProduced T = FinalizedBlockEvents
  type EventsConsumed T = ()
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> DefaultOperationFee fee) <-
      load configPaths
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      finalizedHeightStream <-
        subscribe amqp (Proxy :: Proxy FinalizedHeightValue)
      provisionalBlockEventStream <-
        streamProvisionalBlockEvents dbPool tezos Tezos.defaultFinalizationLag
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , finalizedHeightStream
          , provisionalBlockEventStream
          , defaultOperationFee = fromIntegral fee
          }
  rpcHandlers t = rewardInfo t :<|> monitorOperation t
  masterInstance t@T {defaultOperationFee} = do
    provisionalHeightStream <- streamProvisionalHeight t
    finalizedBlockEventStream <-
      streamFinalizedBlockEvents t provisionalHeightStream
    (finalizedHeightWriter, finalizedHeightStream) <- newStream
    tapStream_
      (\Tezos.BlockEvent {number} -> writeStream finalizedHeightWriter number)
      finalizedBlockEventStream
    operationFeeStream <- streamFromList [defaultOperationFee]
    return
      ( provisionalHeightStream :<|> finalizedHeightStream :<|>
        operationFeeStream
      , (finalizedBlockEventStream, materializeFinalizedBlockEvent t)
      , ())
