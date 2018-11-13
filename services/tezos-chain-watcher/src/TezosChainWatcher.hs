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
import Vest hiding (hash)
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
         let opHashes = fmap toOpHash operations
         when
           (opHash `elem` opHashes)
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
    opBlockInfoMaybe <- Db.runLogged t (selectBlockInfoForOpHash opHash)
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
originatedMapping t = Db.runLogged t . selectOriginatedForImplicit

materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber = do
  atomically $
    readTMVar (lastProducedBlockNumber t) >>= check . (blockNumber <=)
  Db.runLogged t (selectBlockEventByNumber blockNumber) >>= fromRightOrThrowLeft >>=
    fromJustUnsafe BugException

makeBlockEventStream :: T -> IO (Stream QueueBuffer Tezos.BlockEvent)
makeBlockEventStream t = do
  nextBlockNumber <- Db.runLogged t selectNextBlockNumber
  rawBlockEventStream <- Tezos.streamNewBlockEventsDurable (tezos t) (logger t)
  blockEventStream <-
    gapFilledStream
      (Tezos.materializeBlockEventDurable (tezos t) (logger t))
      nextBlockNumber
      rawBlockEventStream
  -- Async persist block events and update block counter.
  let T {lastProducedBlockNumber} = t
  tapStream_
    (\blockEvent -> do
       createdAt <- now
       Db.runLoggedTransaction t (insertBlockEvent createdAt blockEvent)
       let Tezos.BlockEvent {number = newBlockNumber} = blockEvent
       void . atomically $ swapTMVar lastProducedBlockNumber newBlockNumber
       log t Debug ("persisted block number " <> show newBlockNumber))
    blockEventStream
  return blockEventStream

blockEventConsumer :: T -> Consumers BlockEvents
blockEventConsumer t =
  let getInitialBlockNumber = Db.runLogged t selectNextBlockNumber
      T {blockEventConsumerWriter = writer, lastConsumedBlockNumber} = t
      updateBlockNumber Tezos.BlockEvent {number} =
        atomically . swapTMVar lastConsumedBlockNumber $ number
      consumeBlockEvent x = updateBlockNumber x >> writeStream writer x
   in (getInitialBlockNumber, consumeBlockEvent)

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventsProduced T = BlockEvents
  type EventsConsumed T = BlockEvents
  type RpcSpec T = MonitorOpEndpoint
                   :<|> RewardInfoEndpoint
                   :<|> OriginatedMappingEndpoint
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, redisConfig, tezosConfig} <-
      Yaml.decodeFileThrow configFile
    seed <- Yaml.decodeFileThrow seedFile -- TODO: What's up with padding?
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    lastProducedBlockNumber <- newEmptyTMVarIO
    lastConsumedBlockNumber <- newEmptyTMVarIO
    (blockEventConsumerWriter, blockEventConsumerStream) <- newStream
    with
      (PoolConfig (sec 30) 5 dbConfig :<|> amqpConfig :<|> redisConfig :<|>
       tezosConfig) $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $
        T
          { dbPool
          , amqp
          , redis
          , tezos
          , accessControlClient
          , lastProducedBlockNumber
          , lastConsumedBlockNumber
          , blockEventConsumerWriter
          , blockEventConsumerStream
          }
  rpcHandlers t = monitorOp t :<|> rewardInfo t :<|> originatedMapping t
  valuesPublished _ = ()
  eventProducers t = (makeBlockEventStream t, materializeBlockEvent t)
  eventConsumers = blockEventConsumer
