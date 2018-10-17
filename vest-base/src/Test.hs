module Test
  ( module Reexports
  , testCase
  ) where

import Vest

import qualified Data.ByteString.Lazy as LazyByteString
import GHC.Base (String)
import Test.Tasty as Reexports (TestTree, testGroup)
import Test.Tasty.ExpectedFailure as Reexports (expectFail, ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff)

diffCmd :: FilePath -> FilePath -> [GHC.Base.String]
diffCmd ref new = ["diff", "-u", ref, new]

testCase :: String -> FilePath -> IO LazyByteString.ByteString -> TestTree
testCase name = goldenVsStringDiff name diffCmd
