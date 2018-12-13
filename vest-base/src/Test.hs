module Test
  ( module Reexports
  , testCase
  , testCase'
  , testWithResource
  , testWithLoadableResource
  , testWithSandbox
  , testWithService
  , ignoreIO
  , TestService
  ) where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import GHC.Base (String)
import Postgres
import System.IO.Silently (hSilence)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty as Reexports (TestTree, defaultMain, testGroup)
import qualified Test.Tasty as Tasty
import Test.Tasty.ExpectedFailure as Reexports (ignoreTest)
import Test.Tasty.Golden (goldenVsStringDiff)
import Vest

sandboxLock :: Lock
{-# NOINLINE sandboxLock #-}
sandboxLock = unsafePerformIO Lock.new

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", "--color", ref, new]

testCase :: Text -> Path Rel File -> IO Text -> TestTree
testCase = testCase'

testCase' ::
     ConvertibleStrings s LazyByteString
  => Text
  -> Path Rel File
  -> IO s
  -> TestTree
testCase' name path test =
  goldenVsStringDiff (unpack name) diffCmd (toFilePath path) $
  (convertString <$> hSilence [stderr] test) `catchAny`
  (return . convertString . show)

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

data Test

instance HasNamespace Test where
  type Namespace Test = "test"

testWithSandbox :: [Pg ()] -> Path Rel Dir -> TestTree -> TestTree
-- ^ Resets amqp, redis, blockchain, and clears and seeds databases
-- TODO: support >1 db
testWithSandbox dbSeed configDir test =
  Tasty.withResource
    (do Lock.acquire sandboxLock
        dir <- getCurrentDir
        -- clear amqp
        -- clear redis
        -- reset blockchain state
        withLoadable [dir </> configDir] $ \(db :: Pool (Specific Test Connection)) -> do
          runLoggedTransaction db $ do forM_ dbSeed identity -- also clear db
        return ())
    (\() -> Lock.release sandboxLock)
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
