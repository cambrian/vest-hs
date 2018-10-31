module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

-- import TezosDelegationCore.Api as TezosDelegationCore
import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import Db
import TezosDelegationCore.Internal as TezosDelegationCore
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { dbConfig :: PostgresConfig
  , amqpConfig :: Amqp.Config
  , redisConfig :: RedisConfig
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
  help "Payout and refund server for the delegation marketplace." &=
  summary "tezos-delegation-core v0.1.0" &=
  program "tezos-delegation-core"

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
    Config {dbConfig, amqpConfig, redisConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    with3
      dbConfig
      amqpConfig
      redisConfig
      (\(db, amqp, redis) ->
         with
           AccessControlClient.Config {accessControlPublicKey, seed, amqp}
           (\accessControlClient -> f $ T {db, amqp, redis, accessControlClient}))
