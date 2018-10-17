import qualified AccessControl
import qualified AccessControl.Auth
import qualified AccessControl.Handlers
import qualified AccessControl.Permission as Permission
import qualified AccessControl.TestClient
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import Test
import qualified Transport.Amqp as Amqp
import Vest

seedFile :: FilePath
seedFile = "access-control/test/access-control-seed.yaml"

pubKeyFile :: FilePath
pubKeyFile = "access-control/test/access-control-public-key.yaml"

subjectsFile :: FilePath
subjectsFile = "access-control/test/subjects.yaml"

data TestServer = TestServer
  { amqp :: Amqp.T
  , accessControlPublicKey :: PublicKey
  }

instance HasRpcTransport Amqp.T TestServer where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Auth.HasVerifier TestServer where
  accessControlPublicKey = accessControlPublicKey

type PermittedEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.A)) TestServer Amqp.T "permittedEndpoint" () ('Direct ())

type ForbiddenEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.B)) TestServer Amqp.T "forbiddenEndpoint" () ('Direct ())

instance Service TestServer where
  type ServiceArgs TestServer = ()
  type RpcSpec TestServer = PermittedEndpoint
                            :<|> ForbiddenEndpoint
  type PubSubSpec TestServer = ()
  defaultArgs = ()
  init () f = do
    accessControlPublicKey <- Yaml.decodeFileThrow pubKeyFile
    with Amqp.localConfig $ \amqp ->
      f $ TestServer {amqp, accessControlPublicKey}

generatePublicKey :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generatePublicKey =
  testCase "Generate Access Control Public Key" pubKeyFile $ do
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    (publicKey, _) <- seedKeyPairUnsafe seed
    return $ Yaml.encode publicKey

generateSubjects :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generateSubjects =
  testCase "Generate Access Control Subjects" subjectsFile $ do
    (publicKey, _) <- seedKeyPairUnsafe AccessControl.TestClient.seed
    let subjects =
          HashMap.fromList
            [ ( publicKey
              , AccessControl.Subject
                  { AccessControl.name =
                      untag $ namespace @AccessControl.TestClient.T
                  , AccessControl.permissions = HashSet.fromList [Permission.A]
                  })
            ]
    return $ Yaml.encode subjects

testPermitted :: IO AccessControl.TestClient.T -> TestTree
testPermitted t =
  testCase "Permitted" "access-control/test/permitted.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy PermittedEndpoint)
    call () >>- show

testForbidden :: IO AccessControl.TestClient.T -> TestTree
testForbidden t =
  expectFail $
  testCase "Forbidden" "access-control/test/forbidden.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy ForbiddenEndpoint)
    call () >>- show

accessControlServiceConfig :: ServiceResourceConfig AccessControl.T
accessControlServiceConfig =
  ServiceResourceConfig
    { serviceResourceArgs =
        AccessControl.Args
          { AccessControl.subjectsFile = subjectsFile
          , AccessControl.seedFile = seedFile
          , AccessControl.tokenTTLHours = 1
          }
    , serviceResourceStreams = const $ return ()
    , serviceResourceHandlers = AccessControl.Handlers.t
    }

handler :: TestServer -> AccessControl.Auth.Claims -> () -> IO ()
handler _ _ _ = return ()

testServerConfig :: ServiceResourceConfig TestServer
testServerConfig =
  ServiceResourceConfig
    { serviceResourceArgs = ()
    , serviceResourceStreams = const $ return ()
    , serviceResourceHandlers = handler :<|> handler
    }

generateDataFiles :: TestTree
generateDataFiles =
  testGroup "Generate data files" [generatePublicKey, generateSubjects]

tests :: TestTree
tests =
  testWithResource @(ServiceResource AccessControl.T) accessControlServiceConfig $
  const $
  testWithResource @(ServiceResource TestServer) testServerConfig $
  const $
  testWithResource
    AccessControl.TestClient.Config {amqpConfig = Amqp.localConfig} $ \testClient ->
    testGroup "Tests" [testPermitted testClient, testForbidden testClient]

main :: IO ()
main = defaultMain $ testGroup "All" [generateDataFiles, tests]
