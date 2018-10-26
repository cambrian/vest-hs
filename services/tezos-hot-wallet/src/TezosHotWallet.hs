module TezosHotWallet
  ( module TezosHotWallet
  ) where

-- import TezosHotWallet.Api as TezosHotWallet
import qualified AccessControl.Client as AccessControlClient
import qualified Data.Yaml as Yaml
import qualified Db
import qualified Http
import TezosHotWallet.Internal as TezosHotWallet
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data Config = Config
  { amqpConfig :: Amqp.Config
  , dbConfig :: Db.JsonConfig
  , redisConfig :: RedisJsonConfig
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
  help "Server to transact Tezos on behalf of Vest." &=
  summary "tezos-hot-wallet v0.1.0" &=
  program "tezos-hot-wallet"

type Api = ()

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ServiceArgs T = Args
  type VariableSpec T = ()
  type EventSpec T = ()
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configFile, seedFile, accessControlPublicKeyFile} f = do
    Config {amqpConfig, dbConfig, redisConfig, tezosConfig} <-
      Yaml.decodeFileThrow configFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- Yaml.decodeFileThrow accessControlPublicKeyFile
    let dbConfig_ = Db.toConfig dbConfig
    let redisConfig_ = toRedisConfig redisConfig
    with4
      dbConfig_
      amqpConfig
      redisConfig_
      tezosConfig
      (\(db, amqp, redis, tezos) ->
         with
           AccessControlClient.Config {accessControlPublicKey, seed, amqp}
           (\accessControlClient ->
              f $ T {db, amqp, redis, tezos, accessControlClient}))
