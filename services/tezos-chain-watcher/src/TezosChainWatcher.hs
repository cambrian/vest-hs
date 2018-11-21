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
  finalEventStream <-
    Tezos.streamBlockEventsDurable (tezos t) finalizationLag nextBlockNumber
  -- Synchronously persist block events in the outgoing stream.
  mapMStream
    (return . identity)
    -- (\blockEvent -> do
    --    createdAt <- now
    --    let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
    --    log Debug "persisting block event" newBlockNumber
    --    -- TODO: Correct persistence logic.
    --    Pg.runLoggedTransaction t (insertBlockEvent createdAt blockEvent)
    --    log Debug "persisted block event" newBlockNumber
    --    return blockEvent)
    finalEventStream

streamProvisionalEvent :: T -> IO (Stream QueueBuffer ProvisionalEvent)
streamProvisionalEvent t = do
  nextBlockNumber <- Pg.runLogged t $ selectNextBlockNumber Provisional
  -- nextBlockProvisionalId <- Pg.runLogged t selectNextBlockProvisionalId
  -- provisionalIdVar <- newTVarIO nextBlockProvisionalId
  provisionalEventStream <-
    Tezos.streamBlockEventsDurable (tezos t) 0 nextBlockNumber
  -- Synchronously persist provisional events in the outgoing stream.
  mapMStream
    (return . (0, ))
    -- (\blockEvent -> do
    --    createdAt <- now
    --    let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
    --    log Debug "persisting block event" newBlockNumber
    --    -- TODO: Correct persistence logic.
    --    Pg.runLoggedTransaction t (insertBlockEvent createdAt blockEvent)
    --    log Debug "persisted block event" newBlockNumber
    --    return blockEvent)
    provisionalEventStream

-- | This service consumes block events for any threads that would normally listen on the raw event
-- stream. Storing the raw event stream is only possible on the master service replica (the one that
-- produces the stream).
blockEventConsumer :: T -> Consumers BlockEvents
blockEventConsumer t =
  let getInitialBlockNumber = Pg.runLogged t $ selectNextBlockNumber Final
      T {blockEventConsumerWriter = writer, lastConsumedFinalBlockNumber} = t
      updateBlockNumber Tezos.BlockEvent {number} =
        atomically . writeTMVar lastConsumedFinalBlockNumber $ number
      consumeBlockEvent x = updateBlockNumber x >> writeStream writer x
   in (getInitialBlockNumber, consumeBlockEvent)

provisionalEventConsumer :: T -> Consumers ProvisionalBlockEvents
provisionalEventConsumer t =
  let getInitialProvisionalId = Pg.runLogged t selectNextBlockProvisionalId
      T { provisionalEventConsumerWriter = writer
        , lastConsumedBlockProvisionalId
        } = t
      updateBlockProvisionalId (provisionalId, _) =
        atomically . writeTMVar lastConsumedBlockProvisionalId $ provisionalId
      consumeProvisionalEvent x =
        updateBlockProvisionalId x >> writeStream writer x
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
