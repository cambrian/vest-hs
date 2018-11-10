module TezosChainWatcher
  ( module TezosChainWatcher
  ) where

import qualified AccessControl.Client
import qualified Data.Yaml as Yaml
import qualified Db
import qualified Http
import qualified Tezos
import TezosChainWatcher.Api as TezosChainWatcher
import TezosChainWatcher.Db
import TezosChainWatcher.Internal as TezosChainWatcher
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: Db.Config
  , amqpConfig :: Amqp.Config
  , redisConfig :: RedisConfig
  , tezosConfig :: Http.Config
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
  , seedFile :: FilePath
  , accessControlPublicKeyFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configFile =
        "config.yaml" &= help "YAML config file" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    , accessControlPublicKeyFile =
        "access-control-public-key.yaml" &=
        help "YAML access control public key file" &=
        explicit &=
        CmdArgs.name "key" &=
        CmdArgs.name "k" &=
        typFile
    } &=
  help "Monitoring server for the Tezos blockchain." &=
  summary "tezos-chain-watcher v0.1.0" &=
  program "tezos-chain-watcher"

tezosOpToBlockHash :: Tezos.Operation -> Tezos.OperationHash
tezosOpToBlockHash (Tezos.OriginationOp Tezos.Origination {hash}) = hash
tezosOpToBlockHash (Tezos.TransactionOp Tezos.Transaction {hash}) = hash
tezosOpToBlockHash (Tezos.Other hash) = hash

-- TODO: Determine a good value for this.
confirmationThreshold :: Word64
confirmationThreshold = 5

monitorOpResponse ::
     Maybe (Word64, Tezos.BlockHash) -> Maybe Word64 -> MonitorOpResponse
monitorOpResponse _ Nothing = MonitorOpResponse Rejected 0 Nothing
monitorOpResponse Nothing _ = MonitorOpResponse Pending 0 Nothing
monitorOpResponse (Just (opBlockNumber, opBlockHash)) (Just currentBlockNumber) =
  let confirmations = currentBlockNumber - opBlockNumber
      status =
        if confirmations >= confirmationThreshold
          then Confirmed
          else Pending
   in MonitorOpResponse status confirmations (Just opBlockHash)

-- This is perhaps more complicated than expected, since the first time we see an op can be the
-- result of a database lookup OR a streaming event.
-- monitorOp ::
--      T -> Tezos.OperationHash -> IO (Stream ValueBuffer MonitorOpResponse)
-- monitorOp t opHash = do
--   let T {blockEventStream, lastProcessedBlockNumber} = t
--   blockNumberVar <- newEmptyTMVarIO
--   firstEventVar <- newEmptyTMVarIO
--   (writer, monitorStream) <- newStream
--   -- _ <- takeWhileMStream (\Tezos.BlockEvent {number, operations} -> do
--   --   let opHashes = fmap tezosOpToBlockHash operations
--   --   when (opHash `elem` opHashes) (atomically $ tryPutTMVar blockNumberVar number)
--   --   ) blockEventStream
--   -- Query DB in parallel and generate the first monitor event.
--   async $ do
--     blockInfoMaybe <- Db.runLogged t (selectBlockInfoForOpHash opHash)
--     case blockInfoMaybe of
--       Just (opBlockNumber, opBlockHash) -> do
--         currentBlockNumber <- atomically $ readTMVar lastProcessedBlockNumber
--         when (opBlockNumber > currentBlockNumber) throw BugException
--         void . atomically $ tryPutTMVar blockNumberVar opBlockNumber
--         writeStream writer
--   return monitorStream
rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

originatedMapping :: T -> Tezos.ImplicitAddress -> IO [Tezos.OriginatedAddress]
originatedMapping t = Db.runLogged t . selectOriginatedForImplicit

materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber = do
  atomically $
    readTMVar (lastProcessedBlockNumber t) >>= check . (blockNumber <=)
  Db.runLogged t (selectBlockEventByNumber blockNumber) >>= fromRightOrThrowLeft >>=
    fromJustUnsafe BugException

-- | TODO: Consider making the retry-blocking based on block number.
materializeCycleEvent :: T -> IndexOf Tezos.CycleEvent -> IO Tezos.CycleEvent
materializeCycleEvent t cycleNumber = do
  atomically $
    readTMVar (lastProcessedCycleNumber t) >>= check . (cycleNumber <=)
  Db.runLogged t (selectCycleEventByNumber cycleNumber) >>=
    fromJustUnsafe BugException

makeEventStreams ::
     Pool Db.Connection
  -> Http.T
  -> Logger
  -> TMVar Word64
  -> TMVar Word64
  -> IO ( Stream QueueBuffer Tezos.BlockEvent
        , Stream QueueBuffer Tezos.CycleEvent)
makeEventStreams dbPool tezos initLogger lastProcessedBlockNumber lastProcessedCycleNumber = do
  nextBlockNumber <- Db.runLogged_ initLogger dbPool selectNextBlockNumber
  nextCycleNumber <- Db.runLogged_ initLogger dbPool selectNextCycleNumber
  rawBlockEventStream <- Tezos.streamNewBlockEventsDurable tezos initLogger
  blockEventStream <-
    gapFilledStream
      (Tezos.materializeBlockEventDurable tezos initLogger)
      nextBlockNumber
      rawBlockEventStream
  -- Async persist block events and update block counter.
  tapStream_
    (\blockEvent -> do
       createdAt <- now
       Db.runLoggedTransaction_
         initLogger
         dbPool
         (insertBlockEvent createdAt blockEvent)
       let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
       void . atomically $ swapTMVar lastProcessedBlockNumber newBlockNumber)
    blockEventStream
  -- Dynamically generate cycle stream from block stream.
  cycleEventStream <- Tezos.toCycleEventStream blockEventStream nextCycleNumber
  -- Async update cycle counter.
  tapStream_
    (\Tezos.CycleEvent {number = newCycleNumber} ->
       void . atomically $ swapTMVar lastProcessedCycleNumber newCycleNumber)
    cycleEventStream
  return (blockEventStream, cycleEventStream)

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventsProduced T = BlockEvents
                          :<|> CycleEvents
  type EventsConsumed T = ()
  type RpcSpec T = RewardInfoEndpoint
                   :<|> OriginatedMappingEndpoint
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, redisConfig, tezosConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    lastProcessedBlockNumber <- newEmptyTMVarIO
    lastProcessedCycleNumber <- newEmptyTMVarIO
    with
      (PoolConfig (sec 30) 5 dbConfig :<|> amqpConfig :<|> redisConfig :<|>
       tezosConfig) $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      (blockEventStream, cycleEventStream) <-
        makeEventStreams
          dbPool
          tezos
          (stderrLogger Debug)
          lastProcessedBlockNumber
          lastProcessedCycleNumber
      -- ^ TODO: How to keep this logger in sync with the one on the service?
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , lastProcessedBlockNumber
          , lastProcessedCycleNumber
          , blockEventStream
          , cycleEventStream
          }
  rpcHandlers t = rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t =
    (blockEventStream t, materializeBlockEvent t) :<|>
    (cycleEventStream t, materializeCycleEvent t)
  eventConsumers _ = ()
