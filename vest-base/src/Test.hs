module Test
  ( module Reexports
  , testCase
  , testWithResource
  , testWithService
  , TestService
  , TestServiceConfig(..)
  ) where

import Vest

import qualified Data.ByteString.Lazy as LazyByteString
import GHC.Base (String)
import Test.Tasty as Reexports (TestTree, defaultMain, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.ExpectedFailure as Reexports (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff)

diffCmd :: FilePath -> FilePath -> [GHC.Base.String]
diffCmd ref new = ["diff", "-u", ref, new]

testCase :: String -> FilePath -> IO ByteString -> TestTree
testCase name path =
  goldenVsStringDiff name diffCmd path . fmap LazyByteString.fromStrict

type TestService a = (Async' "ServiceThread" Void, a)

data TestServiceConfig a = TestServiceConfig
  { testServiceArgs :: ServiceArgs a
  , testServiceStreams :: a -> IO (Streams (PubSubSpec a))
  , testServiceHandlers :: Handlers (RpcSpec a)
  }

instance Service a => Resource (TestService a) where
  type ResourceConfig (TestService a) = TestServiceConfig a
  make TestServiceConfig { testServiceArgs = args
                         , testServiceStreams = streams
                         , testServiceHandlers = handlers
                         } = do
    serviceVar <- newEmptyTMVarIO
    serviceThread <-
      async' $ run @a args streams handlers (atomically . putTMVar serviceVar)
    a <- atomically $ takeTMVar serviceVar
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
  => TestServiceConfig a
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithService config f =
  Tasty.withResource
    (make @(TestService a) config)
    (cleanup @(TestService a))
    (f . fmap snd)
