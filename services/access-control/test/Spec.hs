import qualified AccessControl
import qualified AccessControl.Auth
import qualified AccessControl.Handlers
import qualified AccessControl.Permission as Permission
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Transports.Amqp as Amqp
import Vest

testAccessControlSeed :: ByteString
testAccessControlSeed = "01234567890123456789012345678901"

testServiceSeed :: ByteString
testServiceSeed = "0123456789012345678901234567890 "

makeAccessControl ::
     Amqp.T -> HashMap PublicKey AccessControl.Subject -> IO AccessControl.T
-- ^ amqp not used, but is necessary to create AccessControl.T
makeAccessControl amqp subjects = do
  (publicKey, secretKey) <- seedKeyPairUnsafe testAccessControlSeed
  return $
    AccessControl.T {subjects, amqp, publicKey, secretKey, tokenTTL = hour 1}

data T = T
  { amqp :: Amqp.T
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , accessControlPublicKey :: PublicKey
  , accessToken :: AccessControl.Token
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Auth.HasSigner T where
  secretKey = secretKey
  accessToken = accessToken

instance AccessControl.Auth.HasVerifier T where
  accessControlPublicKey = accessControlPublicKey

type PermittedEndpoint
   = Endpoint "Haskell" ('Auth (AccessControl.Auth.T 'Permission.A)) T Amqp.T "permittedEndpoint" () ('Direct ())

type NotPermittedEndpoint
   = Endpoint "Haskell" ('Auth (AccessControl.Auth.T 'Permission.B)) T Amqp.T "notPermittedEndpoint" () ('Direct ())

handler :: T -> AccessControl.Auth.Claims -> () -> IO ()
handler _ _ _ = return ()

instance Service T where
  type ServiceArgs T = ()
  type RpcSpec T = PermittedEndpoint
                   :<|> NotPermittedEndpoint
  type PubSubSpec T = ()
  defaultArgs = ()
  init _args f = do
    (accessControlPublicKey, _) <- seedKeyPairUnsafe testAccessControlSeed
    (publicKey, secretKey) <- seedKeyPairUnsafe testServiceSeed
    let subject =
          ( publicKey
          , AccessControl.Subject
              {name = "test", permissions = HashSet.fromList [Permission.A]})
        subjects = HashMap.fromList [subject]
    with
      Amqp.localConfig
      (\amqp -> do
         let _ :<|> getAccessToken = AccessControl.Handlers.t
         accessControl <- makeAccessControl amqp subjects
         accessToken <- getAccessToken accessControl publicKey
         f $ T {amqp, publicKey, secretKey, accessControlPublicKey, accessToken})

run :: T -> IO ()
run t = do
  serve (handler :<|> handler) t (Proxy :: Proxy (RpcSpec T))
  let callPermitted :<|> _callNotPermitted =
        makeClient t (Proxy :: Proxy (RpcSpec T))
  callPermitted (sec 0.1) ()
  putText "yay"
  -- callNotPermitted (sec 0.1) ()

main :: IO ()
main = init () run
