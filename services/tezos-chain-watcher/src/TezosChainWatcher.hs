-- TODO: Add more logging.
module TezosChainWatcher
  ( module TezosChainWatcher
  ) where

import qualified AccessControl.Client
import qualified Postgres as Pg
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import TezosChainWatcher.Api as TezosChainWatcher
import TezosChainWatcher.Db
import TezosChainWatcher.Internal as TezosChainWatcher
import Vest hiding (hash)

mapEventsToValues ::
     Eq b => (a -> b) -> Stream QueueBuffer a -> IO (Stream ValueBuffer b)
mapEventsToValues f eventStream = do
  (valueWriter, valueStream) <- newStream
  tapStream_ (writeStream valueWriter . f) eventStream
  return valueStream

rewardInfo ::
     T
  -> RewardInfoRequest
  -> IO (Either Tezos.InvalidCycleException [Tezos.RewardInfo])
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

-- | This implementation of monitor op, while conceptually simple, may drop confirmation values
-- every now and then. It also requires a DB query per block to materialize op status.
monitorOperation ::
     T -> Tezos.OperationHash -> IO (Stream ValueBuffer Tezos.OperationStatus)
monitorOperation T {dbPool, provisionalHeightStream} opHash = do
  let threshold = Tezos.defaultFinalizationLag
  startHeight <-
    fromJustUnsafe BugException =<< streamNext provisionalHeightStream
  (statusWriter, statusStream) <- newStream
  let materializeOpStatus currentHeight = do
        let monitorDuration =
              if currentHeight > startHeight
                then currentHeight - startHeight
                else 0
        -- ^ If the chain forked and we are still catching up, treat the monitored duration as 0.
        blockInfo <- Pg.runLogged dbPool $ selectBlockInfoForOpHash opHash
        case blockInfo of
          Nothing ->
            if monitorDuration < threshold
              then return $ Tezos.NotIncluded monitorDuration
              else return Tezos.Rejected
            -- ^ Op not seen in valid blocks.
          Just (blockHash, blockHeight) -> do
            let (confirmations :: Int) =
                  fromIntegral currentHeight - fromIntegral blockHeight
            if confirmations < 0
              then return $ Tezos.NotIncluded monitorDuration
              -- ^ Op seen, but the chain forked and we are still catching up.
              else if confirmations < fromIntegral threshold
                     then return $
                          Tezos.Included (blockHash, fromIntegral confirmations)
                     else return $ Tezos.Confirmed blockHash
  -- Materialize op status every time we get a height value.
  tapWhileStream_
    (\height -> do
       opStatus <- materializeOpStatus height
       writeStream statusWriter opStatus
       case opStatus of
         Tezos.Confirmed _ -> closeStream statusWriter >> return False
         Tezos.Rejected -> closeStream statusWriter >> return False
         _ -> return True)
    provisionalHeightStream
  return statusStream

-- | This DB materializer waits on persistence of the desired event to occur.
materializeFinalizedBlockEvent ::
     T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeFinalizedBlockEvent T {dbPool, finalizedHeightStream} blockNumber = do
  takeWhileMStream (return . (blockNumber >)) finalizedHeightStream >>=
    waitStream
  Pg.runLogged dbPool (selectFinalizedBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

streamProvisionalBlockEvents ::
     T -> Word64 -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamProvisionalBlockEvents T {dbPool, tezos} finalizationLag = do
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

streamProvisionalHeightEvents ::
     Stream QueueBuffer Tezos.BlockEvent -> IO (Stream QueueBuffer Word64)
streamProvisionalHeightEvents provisionalBlockEventStream = do
  (provisionalHeightWriter, provisionalHeightStream) <- newStream
  async $
    foldMStream
      (\Tezos.BlockEvent {number} height ->
         if number > height
           then do
             writeStream provisionalHeightWriter number
             return number
           else return height)
      (fromIntegral $ max (Tezos.firstReadableBlockNumber - 1) 0)
      provisionalBlockEventStream
  return provisionalHeightStream

streamFinalizedBlockEvents ::
     T -> Stream QueueBuffer Word64 -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamFinalizedBlockEvents T {dbPool} provisionalHeightEventStream = do
  (finalizedEventWriter, finalizedEventStream) <- newStream
  tapStream_
    (\height -> do
       updatedAt <- now
       let maxBlockNumberToFinalize =
             if height > Tezos.defaultFinalizationLag
               then height - Tezos.defaultFinalizationLag
               else 0
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
    provisionalHeightEventStream
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
    let defaultOperationFee = fromIntegral fee
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      provisionalHeightStream <-
        subscribe amqp (Proxy :: Proxy ProvisionalHeightValue)
      finalizedHeightStream <-
        subscribe amqp (Proxy :: Proxy FinalizedHeightValue)
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , provisionalHeightStream
          , finalizedHeightStream
          , defaultOperationFee
          }
  rpcHandlers t = rewardInfo t :<|> monitorOperation t
  masterInstance t@T {defaultOperationFee} = do
    provisionalBlockEventStream <-
      streamProvisionalBlockEvents t Tezos.defaultFinalizationLag
    provisionalHeightEventStream <-
      streamProvisionalHeightEvents provisionalBlockEventStream
    finalizedBlockEventStream <-
      streamFinalizedBlockEvents t provisionalHeightEventStream
    provisionalHeightStream <-
      mapEventsToValues identity provisionalHeightEventStream
    finalizedHeightStream <-
      mapEventsToValues
        (\Tezos.BlockEvent {number} -> number)
        finalizedBlockEventStream
    operationFeeStream <- streamFromList [defaultOperationFee]
    return
      ( provisionalHeightStream :<|> finalizedHeightStream :<|>
        operationFeeStream
      , (finalizedBlockEventStream, materializeFinalizedBlockEvent t)
      , ())
