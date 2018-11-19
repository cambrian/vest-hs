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

opBlockInfo :: T -> Tezos.OperationHash -> IO OpBlockResponse
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

makeBlockEventStream :: T -> IO (Stream QueueBuffer Tezos.BlockEvent)
makeBlockEventStream t = do
  let T {confirmationLag} = t
  nextBlockNumber <- Pg.runLogged t selectNextBlockNumber
  rawBlockEventStream <-
    Tezos.streamNewBlockEventsDurable (tezos t) confirmationLag
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

newtype TezosConfirmationLag =
  TezosConfirmationLag Int
  deriving newtype (FromJSON)

instance Loadable TezosConfirmationLag where
  configFile = [relfile|tezos-confirmation-lag.json|]

instance Service T where
  type ValueSpec T = LatestBlockEventValue
  type EventsProduced T = BlockEvents
  type EventsConsumed T = ()
  type RpcSpec T = OpBlockInfoEndpoint
                   :<|> RewardInfoEndpoint
                   :<|> ImplicitOriginationsEndpoint
  summary = "Tezos Chain Watcher v0.1.0"
  description = "Tezos chain watcher and blockchain cache."
  init configPaths f = do
    (accessControlPublicKey :<|> seed :<|> confirmationLag) <- load configPaths
    lastConsumedBlockNumber <- newEmptyTMVarIO
    withLoadable configPaths $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      Pg.runLogged dbPool $ Pg.ensureSchema checkedSchema
      -- ^ This action should fail if data loss might occur.
      f $ T {dbPool, amqp, redis, tezos, accessControlClient, confirmationLag}
  rpcHandlers t = opBlockInfo t :<|> rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t = (makeBlockEventStream t, materializeBlockEvent t)
  eventConsumers _ = ()
