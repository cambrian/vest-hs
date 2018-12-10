module Test
  ( module Reexports
  , testCase
  , testCaseRaw
  , testWithResource
  , testWithLoadableResource
  , testWithSeededDb
  , testWithService
  , ignoreIO
  , TestService
  ) where

import Vest

import GHC.Base (String)
import Postgres
import Test.Tasty as Reexports (TestTree, defaultMain, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.ExpectedFailure as Reexports (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff)

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", "--color", ref, new]

testCase :: String -> FilePath -> IO Text -> TestTree
testCase name path = testCaseRaw name path . map convertString

testCaseRaw :: String -> FilePath -> IO ByteString -> TestTree
testCaseRaw name path = goldenVsStringDiff name diffCmd path . map convertString

newtype TestService a = TestService
  { serviceThread :: Async Void
  }

instance Service a => Resource (TestService a) where
  type ResourceConfig (TestService a) = [FilePath]
  resourceName = "TestService " <> namespace @a
  make configDirs = do
    paths <- mapM resolveDir' configDirs
    serviceThread <- async $ run @a paths
    return $ TestService {serviceThread}
  cleanup TestService {serviceThread} = cancel serviceThread

testWithResource ::
     forall a. Resource a
  => ResourceConfig a
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithResource config =
  Tasty.withResource (makeLogged @a config) (cleanupLogged @a)

testWithLoadableResource ::
     forall a. LoadableResource a
  => FilePath
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithLoadableResource configDir =
  Tasty.withResource
    (do path <- resolveDir' configDir
        makeLoadable @a [path])
    (cleanupLogged @a)

data TestNamespace

instance HasNamespace TestNamespace where
  type Namespace TestNamespace = "test"

testWithSeededDb ::
     (Pool (Specific TestNamespace Connection) -> IO ())
  -> FilePath
  -> TestTree
  -> TestTree
-- ^ TODO: would be nice to auto check schema, truncate, seed values, but beam is annoying to work with.
testWithSeededDb f configPath test =
  Tasty.withResource
    (do path <- resolveDir' configPath
        db <- makeLoadable @(Pool (Specific TestNamespace Connection)) [path]
        f db
        return db)
    (cleanupLogged @(Pool (Specific TestNamespace Connection)))
    (const test)

testWithService ::
     forall a. Service a
  => [FilePath]
  -> TestTree
  -> TestTree
-- ^ Overrides the service's config dir logic; it will not automatically look for its namespace.
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithService configDirs test =
  testWithResource @(TestService a) configDirs (const test)

ignoreIO :: a -> IO ()
ignoreIO _ = return ()
