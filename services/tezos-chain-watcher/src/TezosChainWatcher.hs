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

rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

-- | Both materializers wait on consumption of the desired event to occur, since that is the only
-- guarantee that a given event has been persisted in the DB (and can be accessed).
materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber = do
  atomically $
    readTMVar (lastConsumedFinalBlockNumber t) >>= check . (blockNumber <=)
  Pg.runLogged t (selectFinalBlockEventByNumber blockNumber) >>=
    fromJustUnsafe BugException

materializeProvisionalEvent ::
     T -> IndexOf ProvisionalEvent -> IO ProvisionalEvent
materializeProvisionalEvent t provisionalId = do
  atomically $
    readTMVar (lastConsumedBlockProvisionalId t) >>= check . (provisionalId <=)
  event <-
    Pg.runLogged t (selectBlockEventByProvisionalId provisionalId) >>=
    fromJustUnsafe BugException
  return (provisionalId, event)

streamBlockEvent :: T -> IO (Stream QueueBuffer Tezos.BlockEvent)
streamBlockEvent t = do
  let T {finalizationLag = TezosFinalizationLag finalizationLag} = t
  nextBlockNumber <- Pg.runLogged t $ selectNextBlockNumber Final
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
         readTMVar (maxConsumedProvisionalBlockNumber t) >>= check . (number >=)
        -- ^ Ensure that we have seen provisional events at this block level.
       log Debug "cleaning old provisional block events" number
       Pg.runLoggedTransaction
         t
         (deleteOldProvisionalBlockEvents deletedAt number)
       log Debug "cleaned old provisional block events" number
       log Debug "persisting block event" number
       finalized <-
         Pg.runLoggedTransaction
           t
           (finalizeProvisionalBlockEvent updatedAt hash)
       unless finalized $ throw BugException
        -- ^ Finalized events must start as provisional events. We might reconsider this in the
        -- future, but this solution minimizes data duplication and mutable state in the DB.
       log Debug "persisted block event" number
       return event)
    finalEventStream

streamProvisionalEvent :: T -> IO (Stream QueueBuffer ProvisionalEvent)
streamProvisionalEvent t = do
  nextBlockNumber <- Pg.runLogged t $ selectNextBlockNumber Provisional
  nextBlockProvisionalId <- Pg.runLogged t selectNextBlockProvisionalId
  nextProvisionalIdVar <- newTVarIO nextBlockProvisionalId
  (provisionalEventStream, _) <-
    Tezos.streamBlockEventsDurable (tezos t) 0 nextBlockNumber
  -- Synchronously persist provisional events in the outgoing stream.
  mapMStream
    (\blockEvent -> do
       createdAt <- now
       let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
       log Debug "persisting provisional block event" newBlockNumber
      -- Atomically read and increment the global provisionalId.
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
  let getInitialBlockNumber = Pg.runLogged t $ selectNextBlockNumber Final
      T {blockEventConsumerWriter = writer, lastConsumedFinalBlockNumber} = t
      updateBlockNumber Tezos.BlockEvent {number} =
        atomically . writeTMVar lastConsumedFinalBlockNumber $ number
      consumeBlockEvent x = updateBlockNumber x >> writeStream writer x
   in (getInitialBlockNumber, consumeBlockEvent)

-- | This consumer tracks and store the last consumed provisional block ID. More importantly, it
-- tracks the maximum block level seen in provisional blocks, which we use to make sure that we
-- don't persist finalized events until they are first seen in a provisional state.
provisionalEventConsumer :: T -> Consumers ProvisionalBlockEvents
provisionalEventConsumer t =
  let getInitialProvisionalId = Pg.runLogged t selectNextBlockProvisionalId
      T { provisionalEventConsumerWriter = writer
        , lastConsumedBlockProvisionalId
        , maxConsumedProvisionalBlockNumber
        } = t
      updateBlockProvisionalId (provisionalId, _) =
        atomically $ writeTMVar lastConsumedBlockProvisionalId provisionalId
      updateProvisionalBlockNumber (_, Tezos.BlockEvent {number}) = do
        provisionalBlockNumber <-
          atomically $ readTMVar maxConsumedProvisionalBlockNumber
        when (number > provisionalBlockNumber) $
          atomically $ writeTMVar maxConsumedProvisionalBlockNumber number
      consumeProvisionalEvent x = do
        updateBlockProvisionalId x
        updateProvisionalBlockNumber x
        writeStream writer x
   in (getInitialProvisionalId, consumeProvisionalEvent)

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
    lastConsumedFinalBlockNumber <- newEmptyTMVarIO
    (blockEventConsumerWriter, blockEventConsumerStream) <- newStream
    lastConsumedBlockProvisionalId <- newEmptyTMVarIO
    maxConsumedProvisionalBlockNumber <- newEmptyTMVarIO
    (provisionalEventConsumerWriter, provisionalEventConsumerStream) <-
      newStream
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
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
