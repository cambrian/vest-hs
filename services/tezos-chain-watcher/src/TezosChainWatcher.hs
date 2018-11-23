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

getNextBlockNumber :: T -> IO Word64
getNextBlockNumber T {lastConsumedFinalBlockNumber} =
  (1 +) <$> readTVarIO lastConsumedFinalBlockNumber

getNextBlockProvisionalId :: T -> IO Int
getNextBlockProvisionalId T {lastConsumedBlockProvisionalId} =
  (1 +) <$> readTVarIO lastConsumedBlockProvisionalId

rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

-- | Both materializers wait on consumption of the desired event to occur, since that is the only
-- guarantee that a given event has been persisted in the DB (and can be accessed).
materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber = do
  atomically $
    readTVar (lastConsumedFinalBlockNumber t) >>= check . (blockNumber <=)
  Pg.runLogged t (selectFinalBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

materializeProvisionalEvent ::
     T -> IndexOf ProvisionalEvent -> IO ProvisionalEvent
materializeProvisionalEvent t provisionalId = do
  atomically $
    readTVar (lastConsumedBlockProvisionalId t) >>= check . (provisionalId <=)
  event <-
    Pg.runLogged t (selectBlockEventByProvisionalId provisionalId) >>=
    fromJustUnsafe BugException
  return (provisionalId, event)

streamBlockEvent :: T -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamBlockEvent t = do
  let T {finalizationLag = TezosFinalizationLag finalizationLag} = t
  nextBlockNumber <- getNextBlockNumber t
  (finalEventStream, invalidationStream) <-
    Tezos.streamBlockEventsDurable (tezos t) finalizationLag nextBlockNumber
  -- Asynchronously invalidate provisional events when necessary.
  tapStream_
    (\invalidations -> do
       deletedAt <- now
       log Debug "invalidating provisional block events" invalidations
       Pg.runLoggedTransaction
         t
         (deleteProvisionalBlockEventsByHash deletedAt invalidations)
       log Debug "invalidated provisional block events" invalidations)
    invalidationStream
  -- Synchronously persist block events in the outgoing stream.
  mapMStream
    (\event -> do
       updatedAt <- now
       let deletedAt = updatedAt
           Tezos.BlockEvent {hash, number} = event
       atomically $
         readTVar (maxConsumedProvisionalBlockNumber t) >>= check . (number <=)
        -- ^ Ensure that we have seen provisional events at at least this block level.
       log Debug "persisting block event" number
       Pg.runLoggedTransaction
         t
         (do finalized <- finalizeProvisionalBlockEvent updatedAt hash
             unless finalized $ liftIO $ throw BugException
             deleteOldProvisionalBlockEvents deletedAt number)
        -- ^ Finalized events must start as provisional events. We might reconsider this in the
        -- future, but this solution minimizes data duplication and mutable state in the DB.
        -- TODO: Verify that finalized events will never be missed as provisional events, even
        -- though the streams for each are run separately.
       log Debug "persisted block event" number
       log Debug "cleaned old provisional block events" number
       return event)
    finalEventStream

streamProvisionalEvent :: T -> IO (Stream QueueBuffer ProvisionalEvent)
streamProvisionalEvent t = do
  nextBlockNumber <- getNextBlockNumber t
  nextBlockProvisionalId <- getNextBlockProvisionalId t
  nextProvisionalIdVar <- newTVarIO nextBlockProvisionalId
  -- ^ Stores an incrementable provisionalId for producing events. Note that the provisionalId
  -- stored on the service type is only incremented when provisional events are consumed.
  (provisionalEventStream, _) <-
    Tezos.streamBlockEventsDurable (tezos t) 0 nextBlockNumber
  -- Synchronously persist provisional events in the outgoing stream.
  mapMStream
    (\blockEvent -> do
       createdAt <- now
       let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
       log Debug "persisting provisional block event" newBlockNumber
       -- Atomically read and increment the local provisionalId.
       provisionalId <-
         atomically $ do
           oldValue <- readTVar nextProvisionalIdVar
           writeTVar nextProvisionalIdVar (oldValue + 1)
           return oldValue
       Pg.runLoggedTransaction
         t
         (insertProvisionalBlockEvent createdAt provisionalId blockEvent)
       log Debug "persisted provisional block event" newBlockNumber
       return (provisionalId, blockEvent))
    provisionalEventStream

-- | This service consumes block events for any threads that would normally listen on the raw event
-- stream. Storing the raw event stream is only possible on the master service replica (the one that
-- produces the stream).
--
-- Note that this consumer stores the last consumed block event number (for any materializers or
-- RPCs that need to block until their data is ready).
blockEventConsumer :: T -> Consumers BlockEvents
blockEventConsumer t =
  let T {blockEventConsumerWriter = writer, lastConsumedFinalBlockNumber} = t
      updateBlockNumber Tezos.BlockEvent {number} =
        atomically . writeTVar lastConsumedFinalBlockNumber $ number
      consumeBlockEvent x = updateBlockNumber x >> writeStream writer x
   in (getNextBlockNumber t, consumeBlockEvent)

-- | This consumer tracks and stores the last consumed block provisional ID. More importantly, it
-- tracks the maximum block level seen in provisional blocks so far, since we don't want to persist
-- finalized events until they are first seen in a provisional state.
provisionalEventConsumer :: T -> Consumers ProvisionalBlockEvents
provisionalEventConsumer t =
  let T { provisionalEventConsumerWriter = writer
        , lastConsumedBlockProvisionalId
        , maxConsumedProvisionalBlockNumber
        } = t
      updateBlockProvisionalId (provisionalId, _) =
        atomically $ writeTVar lastConsumedBlockProvisionalId provisionalId
      updateProvisionalBlockNumber (_, Tezos.BlockEvent {number}) = do
        provisionalBlockNumber <- readTVarIO maxConsumedProvisionalBlockNumber
        when (number > provisionalBlockNumber) $
          atomically $ writeTVar maxConsumedProvisionalBlockNumber number
      consumeProvisionalEvent x =
        updateBlockProvisionalId x >> updateProvisionalBlockNumber x >>
        writeStream writer x
   in (getNextBlockProvisionalId t, consumeProvisionalEvent)

instance Service T where
  type ValueSpec T = ()
  type EventsProduced T = BlockEvents
                          :<|> ProvisionalBlockEvents
  type EventsConsumed T = BlockEvents
                          :<|> ProvisionalBlockEvents
  type RpcSpec T = RewardInfoEndpoint
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> finalizationLag) <- load configPaths
    (blockEventConsumerWriter, blockEventConsumerStream) <- newStream
    (provisionalEventConsumerWriter, provisionalEventConsumerStream) <-
      newStream
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      lastConsumedFinalBlockNumber <-
        Pg.runLogged dbPool (selectMaxBlockNumber Final) >>= newTVarIO
      lastConsumedBlockProvisionalId <-
        Pg.runLogged dbPool selectMaxBlockProvisionalId >>= newTVarIO
      maxConsumedProvisionalBlockNumber <-
        Pg.runLogged dbPool (selectMaxBlockNumber NotDeleted) >>= newTVarIO
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , lastConsumedFinalBlockNumber
          , blockEventConsumerWriter
          , blockEventConsumerStream
          , lastConsumedBlockProvisionalId
          , maxConsumedProvisionalBlockNumber
          , provisionalEventConsumerWriter
          , provisionalEventConsumerStream
          , finalizationLag
          }
  rpcHandlers = rewardInfo
  valuesPublished _ = ()
  eventProducers t =
    (streamBlockEvent t, materializeBlockEvent t) :<|>
    (streamProvisionalEvent t, materializeProvisionalEvent t)
  eventConsumers t = blockEventConsumer t :<|> provisionalEventConsumer t
