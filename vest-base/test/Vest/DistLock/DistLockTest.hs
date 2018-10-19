module Vest.DistLock.DistLockTest
  ( test_dist_lock
  ) where

import Test
import Vest
import qualified Vest.DistLock as Lock
import qualified Vest.Redis as Redis

testLock :: Redis.T -> Text' "LockId" -> ResourceConfig Lock.T
testLock conn lockId = Lock.Config {conn, lockId, renewInterval = sec 1}

simpleConcurrentTest :: TestTree
simpleConcurrentTest =
  testCase "simpleConcurrent" "test/Vest/DistLock/simple-concurrent.gold" $
  with
    @Redis.T
    Redis.localConfig
    (\connection -> do
       let key = "simpleConcurrent"
       result <- newTMVarIO ""
       -- Wait 30 ms then try to acquire lock.
       -- Note that even though this thread starts after threadC, it gets the lock first because
       -- threadC is stuck in a polling sleep. (Unfortunate, but what to do with polling?)
       threadA <-
         async $ do
           threadDelay (ms 30)
           with @Lock.T (testLock connection key) $
             const $
             atomically $ do
               current <- readTMVar result
               swapTMVar result (current ++ "A")
      -- Acquire lock then sleep for 20 ms.
      -- Test renewals by changing this to 3 seconds.
       threadB <-
         async $
         with @Lock.T (testLock connection key) $
         const $ do
           threadDelay (ms 20)
           atomically $ do
             current <- readTMVar result
             swapTMVar result (current ++ "B")
       -- Wait 10 ms then try to acquire lock.
       threadC <-
         async $ do
           threadDelay (ms 10)
           with @Lock.T (testLock connection key) $
             const $
             atomically $ do
               current <- readTMVar result
               swapTMVar result (current ++ "C")
       wait threadA
       wait threadB
       wait threadC
       resultValue <- atomically $ takeTMVar result
       return . encodeUtf8 . pack $ resultValue)

test_dist_lock :: TestTree
test_dist_lock = testGroup "DistLock" [simpleConcurrentTest]
