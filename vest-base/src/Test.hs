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
testCase name path = testCaseRaw name path . fmap convertString

testCaseRaw :: String -> FilePath -> IO ByteString -> TestTree
testCaseRaw name path =
  goldenVsStringDiff name diffCmd path . fmap convertString

type TestService a = (Async' "ServiceThread" Void, a)

instance Service a => Resource (TestService a) where
  type ResourceConfig (TestService a) = ServiceArgs a
  resourceName = "TestService " <> namespace @a
  make args = do
    serviceVar <- newEmptyTMVarIO
    mainThread <- myThreadId
    serviceThread <- async' $ run args (atomically . putTMVar serviceVar)
    exceptionWatcher <-
      async $ do
        threadResult <- waitCatch (untag serviceThread)
        case threadResult of
          Left exc -> evilThrowTo mainThread exc
          Right _ -> return ()
    a <- atomically $ takeTMVar serviceVar
    cancel exceptionWatcher
    return (serviceThread, a)
  cleanup = cancel . untag . fst

testWithResource ::
     forall a. Resource a
  => ResourceConfig a
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithResource config = Tasty.withResource (make @a config) (cleanup @a)

testWithService ::
     forall a. Service a
  => ServiceArgs a
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithService args f =
  Tasty.withResource
    (make @(TestService a) args)
    (cleanup @(TestService a))
    (f . fmap snd)

ignoreIO :: a -> IO ()
ignoreIO _ = return ()
