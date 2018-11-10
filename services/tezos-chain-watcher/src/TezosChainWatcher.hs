-- TODO: Consider adding more data consistency checks in this service.
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

toOpHash :: Tezos.Operation -> Tezos.OperationHash
toOpHash (Tezos.OriginationOp Tezos.Origination {hash}) = hash
toOpHash (Tezos.TransactionOp Tezos.Transaction {hash}) = hash
toOpHash (Tezos.Other hash) = hash

-- TODO: Determine a good value for this.
confirmationThreshold :: Word64
confirmationThreshold = 5

-- | Timeout (in blocks) for monitor responses if the op has not been seen yet.
errorThreshold :: Word64
errorThreshold = 5

-- Params are (start block number, start block hash) and (current block number).
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
monitorOp ::
     T -> Tezos.OperationHash -> IO (Stream ValueBuffer MonitorOpResponse)
monitorOp t opHash = do
  let T {blockEventStream, lastProcessedBlockNumber} = t
  opBlockInfoVar <- newEmptyTMVarIO
  startBlockNumberVar <- newEmptyTMVarIO
  (writer, monitorStream) <- newStream
  -- After the first monitor item, generate monitor items until EITHER (1) the confirmation
  -- threshold is reached (relative to the operation block) OR (2) the error threshold is reached
  -- (relative to the start block) without having seen the operation.
  takeWhileMStream
    (\Tezos.BlockEvent {hash, number, operations} -> do
       startBlockNumber <- atomically $ readTMVar startBlockNumberVar
        -- ^ Wait for first item to be streamed.
       let opHashes = fmap toOpHash operations
       when
         (opHash `elem` opHashes)
         (void . atomically $ tryPutTMVar opBlockInfoVar (number, hash))
       opBlockInfoMaybe <- atomically $ tryReadTMVar opBlockInfoVar
       case opBlockInfoMaybe of
         Nothing ->
           if number - startBlockNumber >= errorThreshold
             then writeStream writer (monitorOpResponse Nothing Nothing) >>
                  return False
             else writeStream writer (monitorOpResponse Nothing (Just number)) >>
                  return True
         Just _ ->
           writeStream writer (monitorOpResponse opBlockInfoMaybe (Just number)) >>
           return True)
    blockEventStream
  -- Query DB in parallel and generate the first monitor item. The takeWhileMStream will not
  -- generate further monitor items until the first item has been generated. (Note that this has to
  -- be async so we do not miss stream events during the DB query).
  async $ do
    opBlockInfoMaybe <- Db.runLogged t (selectBlockInfoForOpHash opHash)
    currentBlockNumber <- atomically (readTMVar lastProcessedBlockNumber)
    sequence_ $ void . atomically . putTMVar opBlockInfoVar <$> opBlockInfoMaybe
    writeStream writer $
      monitorOpResponse opBlockInfoMaybe (Just currentBlockNumber)
    atomically $ putTMVar startBlockNumberVar currentBlockNumber
  return monitorStream

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
  type RpcSpec T = MonitorOpEndpoint
                   :<|> RewardInfoEndpoint
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
  rpcHandlers t = monitorOp t :<|> rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t =
    (blockEventStream t, materializeBlockEvent t) :<|>
    (cycleEventStream t, materializeCycleEvent t)
  eventConsumers _ = ()
