module Test
  ( module Reexports
  , testCase
  , testCaseRaw
  , testWithResource
  , testWithLoadableResource
  , testWithService
  , ignoreIO
  , TestService
  ) where

import Vest

import GHC.Base (String)
import Test.Tasty as Reexports (TestTree, defaultMain, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.ExpectedFailure as Reexports (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff)

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", "--color", ref, new]

testCase :: String -> FilePath -> IO Text -> TestTree
testCase name path = testCaseRaw name path . fmap convertString

testCaseRaw :: String -> FilePath -> IO ByteString -> TestTree
testCaseRaw name path =
  goldenVsStringDiff name diffCmd path . fmap convertString

newtype TestService a = TestService
  { serviceThread :: Async Void
  }

instance Service a => Resource (TestService a) where
  type ResourceConfig (TestService a) = [FilePath]
  resourceName = "TestService " <> namespace @a
  make configDirs = do
    mainThread <- myThreadId
    paths <- mapM resolveDir' configDirs
    serviceThread <- async $ run @a paths return
    -- Rethrow exceptions since nobody's waiting on the serviceThread
    void . async $ do
      threadResult <- waitCatch serviceThread
      case threadResult of
        Left exc -> evilThrowTo mainThread exc
        Right r -> absurd r
    return $ TestService {serviceThread}
  cleanup = cancel . serviceThread

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
