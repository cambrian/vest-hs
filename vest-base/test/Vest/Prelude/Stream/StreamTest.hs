module Vest.Prelude.Stream.StreamTest
  ( test_stream
  ) where

import Test
import Vest

simpleValueTest :: TestTree
-- The reader may skip values; pushing should never block.
-- readLastValue and readLatestValue should always work.
simpleValueTest =
  testCase "simple-value" "test/Vest/Prelude/Stream/simple-value.gold" $ do
    (StreamPusher push close, stream) <- newStream @ValueBuffer @Int
    let push_ = void . push
    push_ 1
    push_ 2
    push_ 3
    threadDelay (sec 0.01)
    t <- async $ foldStream (+) 0 stream
    lastt <- async $ readLastValue stream
    threadDelay (sec 0.01)
    atomically close
    sum <- wait t
    last <- wait lastt
    return $ show (sum, last)

simpleQueueTest :: TestTree
-- Push history should be available to the first consumer
simpleQueueTest =
  testCase "simple-queue" "test/Vest/Prelude/Stream/simple-queue.gold" $ do
    (StreamPusher push close, stream) <- newStream @(QueueBuffer_ 4) @Int
    let push_ = void . push
    push_ 1
    push_ 1
    push_ 1
    threadDelay (sec 0.01)
    t1 <- async $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    push_ 1
    push_ 1
    push_ 1
    threadDelay (sec 0.01)
    t2 <- async $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    replicateM_ 10 (push_ 1)
    atomically close
    threadDelay (sec 0.01)
    l <- listFromStream stream
    sum1 <- wait t1
    sum2 <- wait t2
    return $ show (sum1, sum2, l)

test_stream :: TestTree
test_stream = testGroup "Stream" [simpleValueTest, simpleQueueTest]
