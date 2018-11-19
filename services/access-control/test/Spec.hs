import qualified AccessControl
import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified AccessControl.Permission as Permission
import qualified AccessControl.TestClient as TestClient
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import Test
import qualified Transport.Amqp as Amqp
import Vest

acConfigDir :: FilePath
acConfigDir = "access-control/test/config/access-control"

testConfigDir :: FilePath
testConfigDir = "access-control/test/config"

localConfigDir :: FilePath
localConfigDir = "config/local"

-- | This is a test server that has access controlled endpoints.
-- Not to be confused with AccessControl.TestServer
data TestServer = TestServer
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

instance HasNamespace TestServer where
  type Namespace TestServer = "test-server"

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
  type RpcSpec TestServer = PermittedEndpoint
                            :<|> ForbiddenEndpoint
  type ValueSpec TestServer = ()
  type EventsProduced TestServer = ()
  type EventsConsumed TestServer = ()
  summary = ""
  description = ""
  init paths f = do
    accessControlPublicKey <- load paths
    withLoadable paths $ \amqp -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey "testServerSeed"
      f $ TestServer {amqp, accessControlClient}
  rpcHandlers _ = const return :<|> const return
  valuesPublished _ = ()
  eventProducers _ = ()
  eventConsumers _ = ()

generateTestPublicKey :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generateTestPublicKey =
  testCaseRaw
    "Generate Access Control Public Key - test"
    "access-control/test/config/access-control-public-key.yaml" $ do
    path <- resolveDir' $ testConfigDir <> "/access-control"
    seed <- load [path]
    return $ Yaml.encode $ AccessControl.ACPublicKey $ fst $ seedKeyPair seed

generateSubjects :: TestTree
-- ^ somewhat hacky way to generate the subjects.yaml file
generateSubjects =
  testCaseRaw
    "Generate Access Control Subjects"
    "access-control/test/config/access-control/subjects.yaml" $ do
    let subjects =
          HashMap.fromList
            [ ( TestClient.pubKey
              , AccessControl.Subject
                  { AccessControl.name = namespace @TestClient.T
                  , AccessControl.permissions = HashSet.fromList [Permission.B]
                  })
            ]
    return $ Yaml.encode subjects

generatePublicKey :: Text -> TestTree
-- ^ Even hackier way to generate the access-control-public-key.yaml file for prod and local
-- environments.
-- TODO: this should be a commit hook
generatePublicKey env =
  testCaseRaw
    ("Generate Access Control Public Key - " <> unpack env)
    ("confg/" <> unpack env <> "/access-control-public-key.yaml") $ do
    path <- resolveDir' $ "config/" <> unpack env <> "/access-control"
    seed <- load [path]
    panic "asdfs"
    return $ Yaml.encode $ AccessControl.ACPublicKey $ fst $ seedKeyPair seed

testPermitted :: IO TestClient.T -> TestTree
testPermitted t =
  testCase "Permitted" "access-control/test/permitted.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy PermittedEndpoint)
    show <$> call ()

testForbidden :: IO TestClient.T -> TestTree
testForbidden t =
  expectFail $
  testCase "Forbidden" "access-control/test/forbidden.gold" $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy ForbiddenEndpoint)
    show <$> call ()

generateDataFiles :: TestTree
generateDataFiles =
  testGroup
    "Generate data files"
    [generateTestPublicKey, generateSubjects, generatePublicKey "local"]

tests :: TestTree
tests =
  testWithService @AccessControl.T [acConfigDir, localConfigDir] $
  testWithService @TestServer [testConfigDir, localConfigDir] $
  testWithLoadableResource localConfigDir $ \amqp ->
    let testClient = amqp >>= TestClient.make
     in testGroup "Tests" [testPermitted testClient, testForbidden testClient]

main :: IO ()
main = defaultMain $ testGroup "All" [generateDataFiles, tests]
