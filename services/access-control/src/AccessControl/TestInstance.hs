-- | For convenience in testing other services. use:
-- import qualified AccessControl.TestInstance
--
-- test :: TestTree
-- test = do
--   testWithService @AccessControl.TestInstance () $ do ...
module AccessControl.TestInstance
  ( module AccessControl.TestInstance
  ) where

import AccessControl.Api as AccessControl.TestInstance
import AccessControl.Internal as AccessControl.TestInstance
import qualified AccessControl.Permission
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Transport.Amqp as Amqp
import Vest

testAccessControlSeed :: ByteString
testAccessControlSeed = "testSeed"

testAccessControlPubKey :: PublicKey
testAccessControlPubKey = fst $ seedKeyPair testAccessControlSeed

accessToken :: T -> PublicKey -> IO SignedToken
-- ^ this implementation produces signed tokens with all permissions given
accessToken T {secretKey} publicKey = do
  time <- now
  let name = "testUser"
      permissions =
        HashSet.fromList [(toEnum 0 :: AccessControl.Permission.T) ..]
      token = Token {publicKey, name, permissions, time}
      signedToken = sign' secretKey (show' token)
  return signedToken

instance Service T where
  type ServiceArgs T = ()
  type RpcSpec T = TokenEndpoint
                   :<|> InvalidateAllExistingTokensEndpoint
  type ValueSpec T = TokenVersionValue
  type EventsProduced T = ()
  type EventsConsumed T = ()
  defaultArgs = ()
  init () f = do
    let (publicKey, secretKey) = seedKeyPair testAccessControlSeed
    (tokenTimeWriter, minTokenTime) <- newStream
    let bumpMinTokenTime = now >>= writeStream tokenTimeWriter
    bumpMinTokenTime
    with (defaultRedisConfig :<|> Amqp.localConfig) $ \(redis :<|> amqp) ->
      f $
      T
        { subjects = HashMap.fromList []
        , amqp
        , redis
        , publicKey
        , secretKey
        , minTokenTime
        , bumpMinTokenTime
        }
  rpcHandlers t = accessToken t :<|> (\_claims () -> bumpMinTokenTime t)
  valuesPublished = minTokenTime
  eventProducers _ = ()
  eventConsumers _ = ()
