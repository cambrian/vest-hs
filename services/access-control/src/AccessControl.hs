module AccessControl
  ( module AccessControl
  ) where

import AccessControl.Api as AccessControl
import AccessControl.Internal as AccessControl
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import Vest
import qualified Vest as CmdArgs (name)

data Args = Args
  { configDir :: FilePath
  , seedFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configDir =
        "services/config/local" &= help "Config directory" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typDir
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

instance Service T where
  type ServiceArgs T = Args
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type ValueSpec T = TokenVersionValue
  type EventsProduced T = ()
  type EventsConsumed T = ()
  defaultArgs = defaultArgs_
  init Args {configDir, seedFile} f = do
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    let (publicKey, secretKey) = seedKeyPair seed
        acPublicKey = ACPublicKey publicKey
    subjects <- load configDir
    (tokenTimeWriter, minTokenTime) <- newStream
    let bumpMinTokenTime = now >>= writeStream tokenTimeWriter
    bumpMinTokenTime
    withLoadable configDir $ \(amqp :<|> redis) ->
      f $
      T
        { subjects
        , amqp
        , redis
        , publicKey = acPublicKey
        , secretKey
        , minTokenTime
        , bumpMinTokenTime
        }
  rpcHandlers t = accessToken t :<|> (\_claims () -> bumpMinTokenTime t)
  valuesPublished = minTokenTime
  eventProducers _ = ()
  eventConsumers _ = ()
