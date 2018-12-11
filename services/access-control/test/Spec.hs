import qualified AccessControl
import qualified AccessControl.Auth
import qualified AccessControl.Client
import qualified AccessControl.Permission as Permission
import qualified AccessControl.TestClient as TestClient
import qualified Amqp
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Yaml as Yaml
import Test
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
  , redis :: RedisConnection
  }

instance HasNamespace TestServer where
  type Namespace TestServer = "test-server"

instance Has Amqp.T TestServer where
  get = amqp

instance Has RedisConnection TestServer where
  get = redis

-- There has to be a way to automatically derive this... right?
instance Has AccessControl.Client.T TestServer where
  get = accessControlClient

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
    withLoadable paths $ \(amqp :<|> redis) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey "testServerSeed"
      f $ TestServer {amqp, accessControlClient, redis}
  rpcHandlers _ = const return :<|> const return
  masterInstance _ = return ((), (), ())

generateTestPublicKey :: TestTree
-- ^ somewhat hacky way to generate the public-key.yaml file
generateTestPublicKey =
  testCase'
    "Generate Access Control Public Key - test"
    [relfile|access-control/test/config/access-control-public-key.yaml|] $ do
    path <- resolveDir' $ testConfigDir <> "/access-control"
    seed <- load [path]
    return $ Yaml.encode $ AccessControl.ACPublicKey $ fst $ seedKeyPair seed

generateSubjects :: TestTree
-- ^ somewhat hacky way to generate the subjects.yaml file
generateSubjects =
  testCase'
    "Generate Access Control Subjects"
    [relfile|access-control/test/config/access-control/subjects.yaml|] $ do
    let subjects =
          HashMap.fromList
            [ ( TestClient.pubKey
              , AccessControl.Subject
                  { AccessControl.name = namespace @TestClient.T
                  , AccessControl.permissions = HashSet.fromList [Permission.B]
                  })
            ]
    return $ Yaml.encode subjects

generatePublicKey :: Path Rel Dir -> TestTree
-- ^ Even hackier way to generate the access-control-public-key.yaml file for prod and local
-- environments.
-- TODO: this should be a pre-commit hook
generatePublicKey env =
  testCase'
    ("Generate Access Control Public Key - " <> pack (toFilePath env))
    ([reldir|config|] </> env </> [relfile|access-control-public-key.yaml|]) $ do
    path <- resolveDir' $ "config/" <> toFilePath env <> "/access-control"
    seed <- load [path]
    return $ Yaml.encode $ AccessControl.ACPublicKey $ fst $ seedKeyPair seed

testPermitted :: IO TestClient.T -> TestTree
testPermitted t =
  testCase "Permitted" [relfile|access-control/test/permitted.gold|] $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy PermittedEndpoint)
    show <$> call ()

testForbidden :: IO TestClient.T -> TestTree
testForbidden t =
  testCase "Forbidden" [relfile|access-control/test/forbidden.gold|] $ do
    t <- t
    let call = makeClient t (Proxy :: Proxy ForbiddenEndpoint)
    (show <$> call ()) `catchAny` (return . show)

generateDataFiles :: TestTree
generateDataFiles =
  testGroup
    "Generate data files"
    [ generateTestPublicKey
    , generateSubjects
    , generatePublicKey [reldir|local|]
    -- , generatePublicKey "prod"
    ]

tests :: TestTree
tests =
  testWithService @AccessControl.T [acConfigDir, localConfigDir] $
  testWithService @TestServer [testConfigDir, localConfigDir] $
  testWithLoadableResource localConfigDir $ \amqp ->
    let testClient = amqp >>= TestClient.make
     in testGroup "Tests" [testPermitted testClient, testForbidden testClient]

main :: IO ()
main = defaultMain $ testGroup "All" [generateDataFiles, tests]
