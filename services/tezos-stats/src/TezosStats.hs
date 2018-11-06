-- Currently intended as an executable for mocked testing data.
module TezosStats
  ( module TezosStats
  ) where

-- import qualified AccessControl.Client as AccessControlClient
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Yaml as Yaml
import Db
import TezosStats.Api as TezosStats
import TezosStats.Internal as TezosStats

-- import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
    -- dbConfig :: PostgresConfig
  { webSocketConfig :: WebSocket.Config
  -- , amqpConfig :: Amqp.Config
  -- , redisConfig :: RedisConfig
  , streamDelayMillis :: Natural
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
  , stubDataFile :: FilePath
  -- , seedFile :: FilePath
  -- , accessControlPublicKeyFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configFile =
        "config.yaml" &= help "YAML config file" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , stubDataFile =
        "stub-data.json" &= help "JSON stub data" &= explicit &=
        CmdArgs.name "stub-data" &=
        CmdArgs.name "s" &=
        typFile
    -- , seedFile =
    --     "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
    --     CmdArgs.name "d" &=
    --     typFile
    -- , accessControlPublicKeyFile =
    --     "access-control-public-key.yaml" &=
    --     help "YAML access control public key file" &=
    --     explicit &=
    --     CmdArgs.name "key" &=
    --     CmdArgs.name "k" &=
    --     typFile
    } &=
  help "Front-end stats server for Tezos." &=
  summary "tezos-stats v0.1.0" &=
  program "tezos-stats"

type Api
   = OverviewEndpoint
     :<|> BakersEndpoint
     :<|> ImplicitEndpoint
     :<|> OperationEndpoint

overview :: T -> () -> IO TezosStats.OverviewResponse
overview T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ overviewResponse stubData

bakersFn :: T -> () -> IO TezosStats.BakersResponse
bakersFn T {rawStubData} _ = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  return $ bakersResponse stubData

implicit :: T -> Text' "TzImplicitPkh" -> IO TezosStats.ImplicitResponse
implicit T {rawStubData} implicitPkh = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  case HashMap.lookup implicitPkh (implicitResponse stubData) of
    Nothing -> throw $ CallException "no such implicit PKH"
    Just response -> return response

operation ::
     T
  -> Text' "TzOperationHash"
  -> IO (Stream ValueBuffer TezosStats.OperationResponse)
operation T {rawStubData, streamDelayMillis} opHash = do
  stubData <- deserializeUnsafe @'JSON rawStubData
  case HashMap.lookup opHash (operationResponses stubData) of
    Nothing -> throw $ CallException "no such operation hash"
    Just streamOpList -> do
      (pusher, stream) <- newStream
      async $
        mapM_
          (\x ->
             threadDelay (ms (streamDelayMillis % 1)) >> writeStream pusher x)
          streamOpList >>
        closeStream pusher
      return stream

handlers :: Handlers Api
handlers = overview :<|> bakersFn :<|> implicit :<|> operation

type AuxiliaryTypes
   = Raw Baker
     :<|> Raw DelegateFraction
     :<|> Raw DelegateInfo
     :<|> Raw LedgerOperation
     :<|> Raw LedgerOperationType
     :<|> Raw OriginatedAccount
     :<|> Raw TimestampRate
     :<|> Raw TimestampSize

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configFile, stubDataFile} f = do
    Config {webSocketConfig, streamDelayMillis} <-
      Yaml.decodeFileThrow configFile
    rawStubData <- readFile stubDataFile
    with
      webSocketConfig
      (\webSocket -> f $ T {webSocket, rawStubData, streamDelayMillis})
