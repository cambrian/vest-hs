module Vest.DistributedLock.DistributedLockTest
  ( test_distributed_lock
  ) where

import Test
import Vest

testLock :: RedisConnection -> Text' "LockId" -> DistributedLockConfig
testLock redis lockId =
  DistributedLockConfig {redis, lockId, renewInterval = sec 1}

simpleConcurrentTest :: TestTree
simpleConcurrentTest =
  testCase "simpleConcurrent" "test/Vest/DistributedLock/simple-concurrent.gold" $
  with
    localRedisConfig
    (\connection -> do
       let key = "simpleConcurrent"
       result <- newTMVarIO ""
       -- Wait 30 ms then try to acquire lock.
       -- Note that even though this thread starts after threadC, it gets the lock first because
       -- threadC is stuck in a polling sleep. (Unfortunate, but what to do with polling?)
       threadA <-
         async $ do
           threadDelay (ms 30)
           with @DistributedLock (testLock connection key) $
             const $
             atomically $ do
               current <- readTMVar result
               swapTMVar result (current <> "A")
      -- Acquire lock then sleep for 20 ms.
      -- Test renewals by changing this to 3 seconds.
       threadB <-
         async $
         with @DistributedLock (testLock connection key) $
         const $ do
           threadDelay (ms 20)
           atomically $ do
             current <- readTMVar result
             swapTMVar result (current <> "B")
       -- Wait 10 ms then try to acquire lock.
       threadC <-
         async $ do
           threadDelay (ms 10)
           with @DistributedLock (testLock connection key) $
             const $
             atomically $ do
               current <- readTMVar result
               swapTMVar result (current <> "C")
       wait threadA
       wait threadB
       wait threadC
       atomically $ takeTMVar result)

test_distributed_lock :: TestTree
test_distributed_lock = testGroup "DistributedLock" [simpleConcurrentTest]
