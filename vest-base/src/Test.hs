module Test
  ( module Reexports
  , testCase
  , testWithResource
  , ServiceResource
  , ServiceResourceConfig(..)
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

newtype ServiceResource a =
  ServiceResource (Async' "ServiceThread" Void)

data ServiceResourceConfig a = ServiceResourceConfig
  { serviceResourceArgs :: ServiceArgs a
  , serviceResourceStreams :: a -> IO (Streams (PubSubSpec a))
  , serviceResourceHandlers :: Handlers (RpcSpec a)
  }

instance Service a => Resource (ServiceResource a) where
  type ResourceConfig (ServiceResource a) = ServiceResourceConfig a
  make ServiceResourceConfig { serviceResourceArgs = args
                             , serviceResourceStreams = streams
                             , serviceResourceHandlers = handlers
                             } = do
    serviceThread <- async' $ run @a args streams handlers return
    return $ ServiceResource serviceThread
  cleanup (ServiceResource (Tagged serviceThread)) = cancel serviceThread

testWithResource ::
     forall a. Resource a
  => ResourceConfig a
  -> (IO a -> TestTree)
  -> TestTree
-- We're forced to use the somewhat worse (IO a -> TestTree) rather than (a -> TestTree) by tasty.
testWithResource config = Tasty.withResource (make @a config) (cleanup @a)
