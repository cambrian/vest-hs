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

type Api = RewardInfoEndpoint

rewardInfo :: T -> RewardInfoRequest -> IO [Tezos.RewardInfo]
rewardInfo T {tezos} RewardInfoRequest {cycleNumber, delegates} =
  Tezos.getRewardInfo tezos cycleNumber delegates

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
         (persistBlockEvent createdAt blockEvent)
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
  type RpcSpec T = Api
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
  rpcHandlers = rewardInfo
  valuesPublished _ = ()
  eventProducers t =
    (blockEventStream t, materializeBlockEvent t) :<|>
    (cycleEventStream t, materializeCycleEvent t)
  eventConsumers _ = ()
