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

configFile :: FilePath
configFile = "access-control/test/config.yaml"

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

instance HasNamespace TestServer where
  namespace = "TestServer"

instance HasRpcTransport Amqp.T TestServer where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Client.Has TestServer where
  accessControlClient = accessControlClient

instance HasLogger TestServer where
  logger _ = stderrLogger Debug

type PermittedEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.B)) TestServer Amqp.T "permittedEndpoint" () ('Direct ())

type ForbiddenEndpoint
   = Endpoint ('Auth (AccessControl.Auth.T 'Permission.InvalidateAuthTokens)) TestServer Amqp.T "forbiddenEndpoint" () ('Direct ())

instance Service TestServer where
  type ServiceArgs TestServer = ()
  type RpcSpec TestServer = PermittedEndpoint
                            :<|> ForbiddenEndpoint
  type ValueSpec TestServer = ()
  type EventsProduced TestServer = ()
  type EventsConsumed TestServer = ()
  defaultArgs = ()
  init () f = do
    accessControlPublicKey <- Yaml.decodeFileThrow pubKeyFile
    with Amqp.localConfig $ \amqp -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey testServerSeed
      f $ TestServer {amqp, accessControlClient}
  rpcHandlers _ = const return :<|> const return
  makeValuePublishers _ = return ()
  makeEventProducers _ = return ()
  eventConsumers _ = ()

data TestClient = TestClient
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace TestClient where
  namespace = "TestClient"

instance HasRpcTransport Amqp.T TestClient where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Client.Has TestClient where
  accessControlClient = accessControlClient

instance HasLogger TestClient where
  logger _ = stderrLogger Debug

instance Service TestClient where
  type ServiceArgs TestClient = ()
  type RpcSpec TestClient = ()
  type ValueSpec TestClient = ()
  type EventsProduced TestClient = ()
  type EventsConsumed TestClient = ()
  defaultArgs = ()
  init () f = do
    accessControlPublicKey <- Yaml.decodeFileThrow pubKeyFile
    with Amqp.localConfig $ \amqp -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey testClientSeed
      f $ TestClient {amqp, accessControlClient}
  rpcHandlers _ = ()
  makeValuePublishers _ = return ()
  makeEventProducers _ = return ()
  eventConsumers _ = ()

generatePublicKey :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generatePublicKey =
  testCaseRaw "Generate Access Control Public Key" pubKeyFile $ do
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    return $ Yaml.encode $ fst $ seedKeyPair seed

generateSubjects :: TestTree
-- ^ somewhat hacky way to generate the subjects.yaml file
generateSubjects =
  testCaseRaw "Generate Access Control Subjects" subjectsFile $ do
    let subjects =
          HashMap.fromList
            [ ( fst $ seedKeyPair testClientSeed
              , AccessControl.Subject
                  { AccessControl.name = namespace @TestClient
                  , AccessControl.permissions = HashSet.fromList [Permission.B]
                  })
            ]
    return $ Yaml.encode subjects

testPermitted :: IO TestClient -> TestTree
testPermitted t =
  testCase "Permitted" "access-control/test/permitted.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy PermittedEndpoint)
    show <$> call ()

testForbidden :: IO TestClient -> TestTree
testForbidden t =
  expectFail $
  testCase "Forbidden" "access-control/test/forbidden.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy ForbiddenEndpoint)
    show <$> call ()

generateDataFiles :: TestTree
generateDataFiles =
  testGroup "Generate data files" [generatePublicKey, generateSubjects]

tests :: TestTree
tests =
  testWithService
    @AccessControl.T
    (AccessControl.Args
       { AccessControl.configFile = configFile
       , AccessControl.subjectsFile = subjectsFile
       , AccessControl.seedFile = seedFile
       }) $
  const $
  testWithService @TestServer () $
  const $
  testWithService () $ \testClient ->
    testGroup "Tests" [testPermitted testClient, testForbidden testClient]

main :: IO ()
main = defaultMain $ testGroup "All" [generateDataFiles, tests]
