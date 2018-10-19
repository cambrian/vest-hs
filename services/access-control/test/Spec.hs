import qualified AccessControl
import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified AccessControl.Permission as Permission
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

testClientSeed :: ByteString
-- ^ In practice you should read these from files. Doesn't really matter for tests.
testClientSeed = "01234567890123456789012345678901"

testServerSeed :: ByteString
-- ^ Not actually used, but required for accessControlClient setup
testServerSeed = "0123456789012345678901234567890 "

data TestServer = TestServer
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasRpcTransport Amqp.T TestServer where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Client.Has TestServer where
  accessControlClient = accessControlClient

type PermittedEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.B)) TestServer Amqp.T "permittedEndpoint" () ('Direct ())

type ForbiddenEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.InvalidateAuthTokens)) TestServer Amqp.T "forbiddenEndpoint" () ('Direct ())

instance Service TestServer where
  type ServiceArgs TestServer = ()
  type RpcSpec TestServer = PermittedEndpoint
                            :<|> ForbiddenEndpoint
  type PublishSpec TestServer = ()
  defaultArgs = ()
  init () f = do
    accessControlPublicKey <- Yaml.decodeFileThrow pubKeyFile
    with Amqp.localConfig $ \amqp ->
      with
        AccessControl.Client.Config
          {accessControlPublicKey, seed = testServerSeed, amqp} $ \accessControlClient ->
        f $ TestServer {amqp, accessControlClient}

data TestClient = TestClient
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasRpcTransport Amqp.T TestClient where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Client.Has TestClient where
  accessControlClient = accessControlClient

instance Service TestClient where
  type ServiceArgs TestClient = ()
  type RpcSpec TestClient = ()
  type PublishSpec TestClient = ()
  defaultArgs = ()
  init () f = do
    accessControlPublicKey <- Yaml.decodeFileThrow pubKeyFile
    with Amqp.localConfig $ \amqp ->
      with
        AccessControl.Client.Config
          {accessControlPublicKey, seed = testClientSeed, amqp} $ \accessControlClient ->
        f $ TestClient {amqp, accessControlClient}

generatePublicKey :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generatePublicKey =
  testCase "Generate Access Control Public Key" pubKeyFile $ do
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    (publicKey, _) <- seedKeyPairUnsafe seed
    return $ Yaml.encode publicKey

generateSubjects :: TestTree
-- ^ somewhat hacky way to generate the subjects.yaml file
generateSubjects =
  testCase "Generate Access Control Subjects" subjectsFile $ do
    (publicKey, _) <- seedKeyPairUnsafe testClientSeed
    let subjects =
          HashMap.fromList
            [ ( publicKey
              , AccessControl.Subject
                  { AccessControl.name = untag $ namespace @TestClient
                  , AccessControl.permissions = HashSet.fromList [Permission.B]
                  })
            ]
    return $ Yaml.encode subjects

testPermitted :: IO TestClient -> TestTree
testPermitted t =
  testCase "Permitted" "access-control/test/permitted.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy PermittedEndpoint)
    call () >>- show

testForbidden :: IO TestClient -> TestTree
testForbidden t =
  expectFail $
  testCase "Forbidden" "access-control/test/forbidden.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy ForbiddenEndpoint)
    call () >>- show

accessControlServiceConfig :: TestServiceConfig AccessControl.T
accessControlServiceConfig =
  TestServiceConfig
    { testServiceArgs =
        AccessControl.Args
          { AccessControl.subjectsFile = subjectsFile
          , AccessControl.seedFile = seedFile
          }
    , testServiceStreams = AccessControl.makeStreams
    , testServiceHandlers = AccessControl.handlers
    }

handler :: TestServer -> AccessControl.Auth.Claims -> () -> IO ()
handler _ _ _ = return ()

testServerConfig :: TestServiceConfig TestServer
testServerConfig =
  TestServiceConfig
    { testServiceArgs = ()
    , testServiceStreams = const $ return ()
    , testServiceHandlers = handler :<|> handler
    }

testClientConfig :: TestServiceConfig TestClient
testClientConfig =
  TestServiceConfig
    { testServiceArgs = ()
    , testServiceStreams = const $ return ()
    , testServiceHandlers = ()
    }

generateDataFiles :: TestTree
generateDataFiles =
  testGroup "Generate data files" [generatePublicKey, generateSubjects]

tests :: TestTree
tests =
  testWithService @AccessControl.T accessControlServiceConfig $
  const $
  testWithService @TestServer testServerConfig $
  const $
  testWithService testClientConfig $ \testClient ->
    testGroup "Tests" [testPermitted testClient, testForbidden testClient]

main :: IO ()
main = defaultMain $ testGroup "All" [generateDataFiles, tests]
