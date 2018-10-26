module AccessControl
  ( module AccessControl
  ) where

import AccessControl.Api as AccessControl
import AccessControl.Internal as AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import qualified GHC.Base
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

data RedisConfig = RedisConfig
  { host :: GHC.Base.String
  , port :: Int16
  , auth :: Maybe ByteString
  } deriving (Generic, FromJSON)

data Config = Config
  { amqpConfig :: Amqp.Config
  , redisConfig :: RedisConfig
  } deriving (Generic, FromJSON)

data Args = Args
  { configFile :: FilePath
  , subjectsFile :: FilePath
  , seedFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configFile =
        "config.yaml" &= help "YAML config file" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , subjectsFile =
        "subjects.yaml" &= help "Subjects file" &= explicit &=
        CmdArgs.name "subjects" &=
        CmdArgs.name "s" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    } &=
  help "Internal access control server." &=
  summary "access-control v0.1.0" &=
  program "access-control"

accessToken :: T -> PublicKey -> IO SignedToken
accessToken T {subjects, secretKey} publicKey = do
  time <- now
  let Subject {name, permissions} =
        fromMaybe Subject {name = "unknown", permissions = HashSet.empty} $
        HashMap.lookup publicKey subjects
      token = Token {publicKey, name, permissions, time}
      signedToken = sign' secretKey (show' token)
  return signedToken

handlers :: Handlers (RpcSpec T)
handlers = accessToken :<|> (\T {bumpMinTokenTime} _ () -> bumpMinTokenTime)

makeVariables :: T -> IO (Variables (VariableSpec T))
makeVariables = return . minTokenTimes

instance Service T where
  type ServiceArgs T = Args
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type VariableSpec T = TokenVersionVariable
  type EventSpec T = ()
  defaultArgs = defaultArgs_
  init Args {configFile, subjectsFile, seedFile} f = do
    Config {amqpConfig, redisConfig} <- Yaml.decodeFileThrow configFile
    (subjects :: HashMap PublicKey Subject) <- Yaml.decodeFileThrow subjectsFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    let (publicKey, secretKey) = seedKeyPair seed
    (pushTokenTime, _, minTokenTimes, readMinTokenTime) <- pushStream
    let bumpMinTokenTime = now >>= pushTokenTime
    bumpMinTokenTime
    -- If we could somehow derive Read for ConnectInfo, we could use that directly as the Redis
    -- config type that gets read. Alas...
    let redisConfig_ =
          defaultRedisConfig
            { connectHost = host redisConfig
            , connectPort = toPortId $ port redisConfig
            , connectAuth = auth redisConfig
            }
    with2 redisConfig_ amqpConfig $ \(redis, amqp) ->
      f $
      T
        { subjects
        , amqp
        , redis
        , publicKey
        , secretKey
        , readMinTokenTime
        , minTokenTimes
        , bumpMinTokenTime
        }
