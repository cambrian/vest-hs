module Vest.DistLock.DistLockTest
  ( test_dist_lock
  ) where

import Test
import Vest
import qualified Vest.DistLock as Lock
import qualified Vest.Redis as Redis

simpleConcurrentTest :: TestTree
simpleConcurrentTest =
  testCase "simpleConcurrent" "test/Vest/DistLock/simple-concurrent.gold" $
  with
    @Redis.T
    Redis.localConfig
    (\connection -> do
       let key = "simpleConcurrent"
       result <- newTMVarIO ""
       threadA <-
         async $ do
           threadDelay (ms 100)
           with @Lock.T (Lock.defaultLock connection key) $
             const $
             atomically $ do
               current <- readTMVar result
               swapTMVar result (current ++ "A")
       threadB <-
         async $
         with @Lock.T (Lock.defaultLock connection key) $
         const $ do
           threadDelay (ms 200)
           atomically $ do
             current <- readTMVar result
             swapTMVar result (current ++ "B")
       wait threadA
       wait threadB
       resultValue <- atomically $ takeTMVar result
       return . encodeUtf8 . pack $ resultValue)

-- TODO: Test renewal. You can sort of test it by replacing the delays in simpleConcurrent with 2500
-- ms and 7500 ms (the renewInterval is 5 seconds and the ttl is 10 seconds by default).
test_dist_lock :: TestTree
test_dist_lock = testGroup "DistLock" [simpleConcurrentTest]
