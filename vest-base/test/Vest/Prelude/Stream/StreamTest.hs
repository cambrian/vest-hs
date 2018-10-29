module Vest.Prelude.Stream.StreamTest
  ( test_stream
  ) where

import Test
import Vest

-- TODO: refactor these tests to show exactly which elements were pushed/received
simpleValue :: TestTree
-- The reader may skip values; pushing should never block.
-- readLastValue and readLatestValue should always work.
simpleValue =
  testCase "simple-value" "test/Vest/Prelude/Stream/simple-value.gold" $ do
    (pusher, stream) <- newStream @ValueBuffer @Int
    pushStream pusher 1
    pushStream pusher 2
    pushStream pusher 3
    threadDelay (sec 0.01)
    t <- async $ readFinalValue stream
    threadDelay (sec 0.01)
    pushStream pusher 4
    pushStream pusher 5
    pushStream pusher 6
    closeStream pusher
    v <- wait t
    return $ show v

multiValue :: TestTree
multiValue =
  testCase "multi-value" "test/Vest/Prelude/Stream/multi-value.gold" $ do
    (pusher, stream) <- newStream @ValueBuffer @Int
    pushStream pusher 1
    pushStream pusher 2
    pushStream pusher 3
    threadDelay (sec 0.01)
    t1 <- async $ readFinalValue stream
    stream2 <- mapMStream (return . (* 2)) stream
    stream3 <- mapMStream (return . (* 3)) stream
    t2 <- async $ readFinalValue stream2
    threadDelay (sec 0.01)
    pushStream pusher 4
    pushStream pusher 5
    pushStream pusher 6
    closeStream pusher
    threadDelay (sec 0.01)
    v1 <- wait t1
    v2 <- wait t2
    v3 <- readFinalValue stream3
    return $ show (v1, v2, v3)

simpleQueue :: TestTree
-- Push history should be available to the first consumer.
-- Pushing should block if the backing queue is full.
simpleQueue =
  testCase "simple-queue" "test/Vest/Prelude/Stream/simple-queue.gold" $ do
    (pusher, stream) <- newStream @(QueueBuffer_ 4) @Int
    pushStream pusher 1
    pushStream pusher 1
    pushStream pusher 1
    threadDelay (sec 0.01)
    t1 <- async $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    pushStream pusher 1
    pushStream pusher 1
    pushStream pusher 1
    threadDelay (sec 0.01)
    t2 <- async $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    replicateM_ 10 (pushStream pusher 1)
    closeStream pusher
    threadDelay (sec 0.01)
    l <- listFromStream stream
    sum1 <- wait t1
    sum2 <- wait t2
    return $ show (sum1, sum2, l)

multiQueue :: TestTree
multiQueue =
  testCase "multi-queue" "test/Vest/Prelude/Stream/multi-queue.gold" $ do
    (pusher, stream) <- newStream @(QueueBuffer_ 4) @Int
    pushStream pusher 1
    pushStream pusher 1
    pushStream pusher 1
    threadDelay (sec 0.01)
    t1 <- async $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    stream2 <- mapMStream (return . (* 2)) stream
    stream3 <- mapMStream (return . (* 3)) stream
    t2 <- async $ foldStream (+) 0 stream2
    threadDelay (sec 0.01)
    t3 <- async $ foldStream (+) 0 stream3
    threadDelay (sec 0.01)
    pushStream pusher 1
    pushStream pusher 1
    pushStream pusher 1
    closeStream pusher
    threadDelay (sec 0.01)
    sum1 <- wait t1
    sum2 <- wait t2
    sum3 <- wait t3
    return $ show (sum1, sum2, sum3)

toFromList :: TestTree
toFromList =
  testCase "to/from list" "test/Vest/Prelude/Stream/to-from-list.gold" $
  streamFromList @QueueBuffer @Int [1, 2, 3] >>= listFromStream >>- show

test_stream :: TestTree
test_stream =
  testGroup
    "Stream"
    [simpleValue, multiValue, simpleQueue, multiQueue, toFromList]