-- | For convenience in testing other services. This is an instance of the access control service
-- that grants every permission to every user.
--
-- To use:
-- import qualified AccessControl.TestInstance
--
-- test :: TestTree
-- test = do
--   testWithService @AccessControl.TestInstance [localConfigDir] $ do ...
--
-- Also see AccessControl.TestClient for a general purpose client that can hit amqp endpoints.
module AccessControl.TestInstance
  ( module AccessControl.TestInstance
  ) where

import AccessControl.Api as AccessControl.TestInstance
import AccessControl.Internal as AccessControl.TestInstance
import qualified AccessControl.Permission
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

testAccessControlSeed :: Seed
testAccessControlSeed = "testSeed"

testAccessControlPubKey :: PublicKey
testAccessControlPubKey = fst $ seedKeyPair testAccessControlSeed

accessToken :: T -> PublicKey -> IO SignedToken
-- ^ this implementation produces signed tokens with all permissions given
accessToken T {secretKey} publicKey = do
  time <- now
  let name =
        "Test User " <>
        convertString (ByteString.take 8 (unPublicKey publicKey))
      permissions =
        HashSet.fromList [(toEnum 0 :: AccessControl.Permission.T) ..]
      token = Token {publicKey, name, permissions, time}
      signedToken = sign' secretKey (show' token)
  return signedToken

instance Service T where
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type ValueSpec T = TokenVersionValue
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Access Control Test Server"
  description =
    "Test instance for access control service. Grants every permission to every user."
  init configPaths f = do
    let (publicKey, secretKey) = seedKeyPair testAccessControlSeed
    (tokenTimeWriter, minTokenTime) <- newStream
    let bumpMinTokenTime = now >>= writeStream tokenTimeWriter
    bumpMinTokenTime
    withLoadable configPaths $ \(redis :<|> amqp) ->
      f $
      T
        { subjects = HashMap.fromList []
        , amqp
        , redis
        , publicKey = ACPublicKey publicKey
        , secretKey
        , minTokenTime
        , bumpMinTokenTime
        }
  rpcHandlers t = accessToken t :<|> (\_claims () -> bumpMinTokenTime t)
  masterInstance t = return (minTokenTime t, (), ())
