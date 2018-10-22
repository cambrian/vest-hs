module AccessControl
  ( module AccessControl
  ) where

import qualified AccessControl.Auth as Auth
import AccessControl.Internal as AccessControl
import qualified AccessControl.Permission as Permission
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import qualified Transport.Amqp as Amqp
import Vest

data AccessControl = Args
  { subjectsFile :: FilePath
  , seedFile :: FilePath -- Seed must be exactly 32 bytes, encoded in base64.
  } deriving (Data)

type TokenEndpoint
   = Endpoint 'NoAuth T Amqp.T "token" PublicKey ('Direct SignedToken)

type InvalidateAllExistingTokensEndpoint
   = Endpoint ('Auth (Auth.T 'Permission.InvalidateAuthTokens)) T Amqp.T "invalidateAllExistingTokens" () ('Direct ())

type TokenVersionTopic
   = Topic 'Value T Amqp.T "currentTokenVersion" (UUID' "AccessTokenVersion")

accessToken :: T -> PublicKey -> IO SignedToken
accessToken T {subjects, secretKey, tokenVersionVar} publicKey = do
  version <- readTVarIO tokenVersionVar
  let Subject {name, permissions} =
        fromMaybe Subject {name = "unknown", permissions = HashSet.empty} $
        HashMap.lookup publicKey subjects
      token = Token {publicKey, name, permissions, version}
      signedToken = sign' secretKey (show' token)
  return signedToken

handlers :: Handlers (RpcSpec T)
handlers = accessToken :<|> (\T {bumpTokenVersion} _ () -> bumpTokenVersion)

makeStreams :: T -> IO (Streams (PublishSpec T))
makeStreams = return . tokenVersions

instance Permission.Is p => HasAuthVerifier (Auth.T p) T where
  authVerifier T {publicKey, tokenVersionVar} =
    Auth.Verifier publicKey (readTVar tokenVersionVar)

instance Service T where
  type ServiceArgs T = AccessControl
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type PublishSpec T = TokenVersionTopic
  defaultArgs = Args {subjectsFile = "subjects.yaml", seedFile = "seed.yaml"}
  init Args {subjectsFile, seedFile} f = do
    (subjects :: HashMap PublicKey Subject) <- Yaml.decodeFileThrow subjectsFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    (publicKey, secretKey) <- seedKeyPairUnsafe seed
    (pushTokenVersion, _close, tokenVersions) <- pushStream -- do we need to close tokenVersions?
    let bumpTokenVersion = nextUUID' >>= pushTokenVersion
    bumpTokenVersion
    tokenVersionVar <- tvarFromStreamUnsafe tokenVersions
    with localRedisConfig $ \redis ->
      with Amqp.localConfig $ \amqp ->
        f $
        T
          { subjects
          , amqp
          , redis
          , publicKey
          , secretKey
          , tokenVersionVar
          , tokenVersions
          , bumpTokenVersion
          }
