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

opBlockInfo :: T -> Tezos.OperationHash -> IO OpBlockInfoResponse
opBlockInfo t opHash = Pg.runLogged t $ selectBlockInfoForOpHash opHash

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

makeBlockEventStream ::
     T -> Int -> Bool -> IO (Stream QueueBuffer Tezos.BlockEvent)
makeBlockEventStream t confirmationLag persist = do
  nextBlockNumber <- Pg.runLogged t selectNextBlockNumber
  rawBlockEventStream <-
    Tezos.streamNewBlockEventsDurable (tezos t) confirmationLag
  blockEventStream <-
    gapFilledStream
      (Tezos.materializeBlockEventDurable $ tezos t)
      nextBlockNumber
      rawBlockEventStream
  -- Synchronously persist block events.
  if persist
    then mapMStream
           (\blockEvent -> do
              createdAt <- now
              let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
              log Debug "persisting block event" newBlockNumber
              Pg.runLoggedTransaction t (insertBlockEvent createdAt blockEvent)
              log Debug "persisted block event" newBlockNumber
              return blockEvent)
           blockEventStream
    else return blockEventStream

-- | This service consumes block events for any RPCs that would normally listen on the raw event
-- stream. Storing the raw event stream is only possible on the master service replica (the one that
-- produces the stream).
--
-- Right now the only consumer of this is materializeBlockEvent, which blocks until the DB has
-- caught up to the appropriate block number (as indicated by the event stream).
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
                          :<|> ProvisionalBlockEvents
  type EventsConsumed T = BlockEvents
  type RpcSpec T = OpBlockInfoEndpoint
                   :<|> RewardInfoEndpoint
                   :<|> ImplicitOriginationsEndpoint
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> finalizationLag) <- load configPaths
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
          , finalizationLag
          }
  rpcHandlers t = opBlockInfo t :<|> rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t =
    let T {finalizationLag = TezosFinalizationLag lagValue} = t
     in (makeBlockEventStream t lagValue True, materializeBlockEvent t) :<|>
        (makeBlockEventStream t 0 False, materializeBlockEvent t)
  eventConsumers = blockEventConsumer
