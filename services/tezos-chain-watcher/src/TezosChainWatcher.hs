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

type Api = ()

materializeBlockEvent :: T -> IndexOf Tezos.BlockEvent -> IO Tezos.BlockEvent
materializeBlockEvent t blockNumber =
  Db.runLogged t (selectBlockEventByNumber blockNumber) >>= fromRightOrThrowLeft >>=
  fromJustUnsafe BugException

materializeCycleEvent :: T -> IndexOf Tezos.CycleEvent -> IO Tezos.CycleEvent
materializeCycleEvent t blockNumber =
  Db.runLogged t (selectCycleEventByNumber blockNumber) >>=
  fromJustUnsafe BugException

-- TODO: Tap block events and persist. Also change the steady state thing to be the last processed
-- block/cycle number, so we block on implicit materialization until the DB has caught up.
makeProducers ::
     T
  -> IO (Producers (BlockEvents
                    :<|> CycleEvents))
makeProducers t = do
  let T {tezos} = t
  nextBlockNumber <- Db.runLogged t selectNextBlockNumber
  nextCycleNumber <- Db.runLogged t selectNextCycleNumber
  rawBlockEventStream <- Tezos.streamNewBlockEventsDurable tezos (logger t)
  blockEventStream <-
    gapFilledStream
      (Tezos.materializeBlockEventDurable tezos (logger t))
      nextBlockNumber
      rawBlockEventStream
  cycleEventStream <- Tezos.toCycleEventStream blockEventStream nextCycleNumber
  return $
    (blockEventStream, materializeBlockEvent t) :<|>
    (cycleEventStream, materializeCycleEvent t)

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
    reachedSteadyState <- newEmptyTMVarIO
    with
      (PoolConfig (sec 30) 5 dbConfig :<|> amqpConfig :<|> redisConfig :<|>
       tezosConfig) $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $
        T {dbPool, amqp, redis, tezos, accessControlClient, reachedSteadyState}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers = panic "unimplemented"
  eventConsumers _ = ()
