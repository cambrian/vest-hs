module AccessControl
  ( module AccessControl
  ) where

import AccessControl.Api as AccessControl
import AccessControl.Internal as AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import qualified Transport.Amqp as Amqp
import Vest
import qualified Vest as CmdArgs (name)

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

makeValues :: T -> IO (Values (ValueSpec T))
makeValues = return . minTokenTime

instance Service T where
  type ServiceArgs T = Args
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type ValueSpec T = TokenVersionValue
  type EventsProduced T = ()
  type EventsConsumed T = ()
  defaultArgs = defaultArgs_
  init Args {configFile, subjectsFile, seedFile} f = do
    Config {amqpConfig, redisConfig} <- Yaml.decodeFileThrow configFile
    (subjects :: HashMap PublicKey Subject) <- Yaml.decodeFileThrow subjectsFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    let (publicKey, secretKey) = seedKeyPair seed
    (tokenTimeWriter, minTokenTime) <- newStream
    let bumpMinTokenTime = now >>= writeStream tokenTimeWriter
    bumpMinTokenTime
    with (redisConfig :<|> amqpConfig) $ \(redis :<|> amqp) -> do
      let t =
            T
              { subjects
              , amqp
              , redis
              , publicKey
              , secretKey
              , minTokenTime
              , bumpMinTokenTime
              }
          handlers = accessToken t :<|> (\_claims () -> bumpMinTokenTime)
      f (t, handlers, minTokenTime, (), ())
