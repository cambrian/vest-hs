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

-- TODO: Determine a good value for this.
confirmationThreshold :: Word64
confirmationThreshold = 5

-- | Timeout (in blocks) for monitor responses if the op has not been seen yet.
rejectionThreshold :: Word64
rejectionThreshold = 5

-- Params are (start block number), (op block number, op block hash), and (current block number).
monitorOpResponse ::
     Word64 -> Maybe (Word64, Tezos.BlockHash) -> Word64 -> MonitorOpResponse
monitorOpResponse startBlockNumber Nothing currentBlockNumber =
  let numBlocksMonitored = currentBlockNumber - startBlockNumber
      status =
        if numBlocksMonitored >= rejectionThreshold
          then Rejected
          else Pending
   in MonitorOpResponse status 0 Nothing
monitorOpResponse _ (Just (opBlockNumber, opBlockHash)) currentBlockNumber =
  let confirmations = currentBlockNumber - opBlockNumber
      status =
        if confirmations >= confirmationThreshold
          then Confirmed
          else Pending
   in MonitorOpResponse status confirmations (Just opBlockHash)

-- This is perhaps more complicated than expected, since the first time we see an op can be the
-- result of a database lookup OR a streaming event.
monitorOp ::
     T -> Tezos.OperationHash -> IO (Stream ValueBuffer MonitorOpResponse)
monitorOp t opHash = do
  let T {blockEventConsumerStream, lastConsumedBlockNumber} = t
  opBlockInfoVar <- newEmptyTMVarIO
  startBlockNumberVar <- newEmptyTMVarIO
  (writer, monitorStream) <- newStream
  -- After the first monitor item, generate monitor items until EITHER (1) the confirmation
  -- threshold is reached (relative to the operation block) OR (2) the rejection threshold is
  -- reached (relative to the start block) without having seen the operation.
  monitorItemGenerator <-
    takeWhileMStream
      (\Tezos.BlockEvent {hash, number, operations} -> do
         startBlockNumber <- atomically $ readTMVar startBlockNumberVar
        -- ^ Wait for first item to be streamed.
         when
           (opHash `elem` operations)
           (void . atomically $ tryPutTMVar opBlockInfoVar (number, hash))
         opBlockInfoMaybe <- atomically $ tryReadTMVar opBlockInfoVar
         let response =
               monitorOpResponse startBlockNumber opBlockInfoMaybe number
         writeStream writer response >> return (status response == Pending))
      blockEventConsumerStream
  -- Query DB in parallel and generate the first monitor item. The takeWhileMStream will not
  -- generate any monitor items until the first item has been generated. (Note that this has to be
  -- async so we do not miss stream events during the DB query). Then wait for the takeWhile to
  -- finish and close the monitorStream.
  async $ do
    opBlockInfoMaybe <- Pg.runLogged t (selectBlockInfoForOpHash opHash)
    currentBlockNumber <- atomically (readTMVar lastConsumedBlockNumber)
    sequence_ $ void . atomically . putTMVar opBlockInfoVar <$> opBlockInfoMaybe
    writeStream writer $
      monitorOpResponse currentBlockNumber opBlockInfoMaybe currentBlockNumber
    atomically $ putTMVar startBlockNumberVar currentBlockNumber
    waitStream monitorItemGenerator >> closeStream writer
  return monitorStream

rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

originatedMapping :: T -> Tezos.ImplicitAddress -> IO [Tezos.OriginatedAddress]
originatedMapping t = Pg.runLogged t . selectOriginatedForImplicit

materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber = do
  atomically $
    readTMVar (lastConsumedBlockNumber t) >>= check . (blockNumber <=)
  Pg.runLogged t (selectBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

makeBlockEventStream :: T -> IO (Stream QueueBuffer Tezos.BlockEvent)
makeBlockEventStream t = do
  nextBlockNumber <- Pg.runLogged t selectNextBlockNumber
  rawBlockEventStream <- Tezos.streamNewBlockEventsDurable $ tezos t
  blockEventStream <-
    gapFilledStream
      (Tezos.materializeBlockEventDurable $ tezos t)
      nextBlockNumber
      rawBlockEventStream
  -- Synchronously persist block events.
  mapMStream
    (\blockEvent -> do
       createdAt <- now
       let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
       log Debug "persisting block event" newBlockNumber
       Pg.runLoggedTransaction t (insertBlockEvent createdAt blockEvent)
       log Debug "persisted block event" newBlockNumber
       return blockEvent)
    blockEventStream

blockEventConsumer :: T -> Consumers BlockEvents
blockEventConsumer t =
  let getInitialBlockNumber = Pg.runLogged t selectNextBlockNumber
      T {blockEventConsumerWriter = writer, lastConsumedBlockNumber} = t
      updateBlockNumber Tezos.BlockEvent {number} =
        atomically . writeTMVar lastConsumedBlockNumber $ number
      consumeBlockEvent x = updateBlockNumber x >> writeStream writer x
   in (getInitialBlockNumber, consumeBlockEvent)

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = BlockEvents
  type EventsConsumed T = BlockEvents
  type RpcSpec T = MonitorOpEndpoint
                   :<|> RewardInfoEndpoint
                   :<|> OriginatedMappingEndpoint
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed) <- load configPaths
    lastConsumedBlockNumber <- newEmptyTMVarIO
    (blockEventConsumerWriter, blockEventConsumerStream) <- newStream
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      -- ^ This action should fail if data loss might occur.
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , lastConsumedBlockNumber
          , blockEventConsumerWriter
          , blockEventConsumerStream
          }
  rpcHandlers t = monitorOp t :<|> rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t = (makeBlockEventStream t, materializeBlockEvent t)
  eventConsumers = blockEventConsumer
