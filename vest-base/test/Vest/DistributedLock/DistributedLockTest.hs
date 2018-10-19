module Vest.DistributedLock.DistributedLockTest
  ( test_distributed_lock
  ) where

import Test
import Vest

testLock :: RedisConnection -> Text' "LockId" -> DistributedLockConfig
testLock redis lockId =
  DistributedLockConfig
    {redis, lockId, renewInterval = sec 1, pollInterval = ms 5}

simpleConcurrentTest :: TestTree
simpleConcurrentTest =
  testCase "simpleConcurrent" "test/Vest/DistributedLock/simple-concurrent.gold" $
  with
    localRedisConfig
    (\connection -> do
       let lockId = "simpleConcurrent"
       result <- newTVarIO ""
       -- Wait 10 ms then try to acquire lock.
       threadA <-
         async $ do
           threadDelay (ms 10)
           with @DistributedLock (testLock connection lockId) $
             const $ atomically $ modifyTVar result (<> "A")
      -- Wait 0 ms, acquire lock, then sleep for 20 ms more.
      -- Test renewals by changing this to 3 seconds.
       threadB <-
         async $
         with @DistributedLock (testLock connection lockId) $
         const $ do
           threadDelay (ms 20)
           atomically $ modifyTVar result (<> "B")
       -- Wait 40 ms then try to acquire lock.
       threadC <-
         async $ do
           threadDelay (ms 40)
           with @DistributedLock (testLock connection lockId) $
             const $ atomically $ modifyTVar result (<> "C")
       wait threadA
       wait threadB
       wait threadC
       readTVarIO result)

test_distributed_lock :: TestTree
test_distributed_lock = testGroup "DistributedLock" [simpleConcurrentTest]
