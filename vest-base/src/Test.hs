module Test
  ( module Reexports
  , testCase
  , testCaseRaw
  , testWithResource
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
testCase name path test =
  testCaseRaw name path . fmap convertString $ (setTestModeUnsafe True >> test)

testCaseRaw :: String -> FilePath -> IO ByteString -> TestTree
testCaseRaw name path =
  goldenVsStringDiff name diffCmd path . fmap convertString

newtype TestService a = TestService
  { serviceThread :: Async Void
  }

instance Service a => Resource (TestService a) where
  type ResourceConfig (TestService a) = ServiceArgs a
  resourceName = "TestService " <> namespace @a
  make args = do
    mainThread <- myThreadId
    serviceThread <- async $ run @a args return
    exceptionWatcher <-
      async $ do
        threadResult <- waitCatch serviceThread
        case threadResult of
          Left exc -> evilThrowTo mainThread exc
          Right _ -> return ()
    cancel exceptionWatcher
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

testWithService ::
     forall a. Service a
  => ServiceArgs a
  -> TestTree
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithService args test = testWithResource @(TestService a) args (const test)

ignoreIO :: a -> IO ()
ignoreIO _ = return ()
