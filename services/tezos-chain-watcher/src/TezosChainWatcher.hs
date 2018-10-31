module TezosChainWatcher
  ( module TezosChainWatcher
  ) where

-- import TezosChainWatcher.Api as TezosChainWatcher
import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import Db
import qualified Http
import TezosChainWatcher.Internal as TezosChainWatcher
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: PostgresConfig
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

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {dbConfig, amqpConfig, redisConfig, tezosConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    with4
      dbConfig
      amqpConfig
      redisConfig
      tezosConfig
      (\(db, amqp, redis, tezos) ->
         with
           AccessControlClient.Config {accessControlPublicKey, seed, amqp}
           (\accessControlClient ->
              f $ T {db, amqp, redis, tezos, accessControlClient}))
