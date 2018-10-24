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

data Args = Args
  { subjectsFile :: FilePath
  , seedFile :: FilePath -- Seed must be exactly 32 bytes, encoded in base64.
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { subjectsFile =
        "subjects.yaml" &= help "Subjects file" &= explicit &=
        CmdArgs.name "subjects" &=
        CmdArgs.name "S" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "Seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "D" &=
        typFile
    } &=
  help "Our internal access control server." &=
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
  init Args {subjectsFile, seedFile} f = do
    (subjects :: HashMap PublicKey Subject) <- Yaml.decodeFileThrow subjectsFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    let (publicKey, secretKey) = seedKeyPair seed
    (pushTokenTime, _, minTokenTimes, readMinTokenTime) <- pushStream
    let bumpMinTokenTime = now >>= pushTokenTime
    bumpMinTokenTime
    with localRedisConfig $ \redis ->
      with Amqp.localConfig $ \amqp ->
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
