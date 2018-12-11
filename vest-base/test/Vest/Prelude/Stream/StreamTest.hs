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
  testCase "simple-value" [relfile|test/Vest/Prelude/Stream/simple-value.gold|] $ do
    (writer, stream) <- newStream @ValueBuffer @Int
    writeStream writer 1
    writeStream writer 2
    writeStream writer 3
    threadDelay (sec 0.01)
    t <- asyncDetached $ readFinalValue stream
    threadDelay (sec 0.01)
    writeStream writer 4
    writeStream writer 5
    writeStream writer 6
    closeStream writer
    v <- wait t
    return $ show v

multiValue :: TestTree
multiValue =
  testCase "multi-value" [relfile|test/Vest/Prelude/Stream/multi-value.gold|] $ do
    (writer, stream) <- newStream @ValueBuffer @Int
    writeStream writer 1
    writeStream writer 2
    writeStream writer 3
    threadDelay (sec 0.01)
    t1 <- asyncDetached $ readFinalValue stream
    stream2 <- mapMStream (return . (* 2)) stream
    stream3 <- mapMStream (return . (* 3)) stream
    t2 <- asyncDetached $ readFinalValue stream2
    threadDelay (sec 0.01)
    writeStream writer 4
    writeStream writer 5
    writeStream writer 6
    closeStream writer
    threadDelay (sec 0.01)
    v1 <- wait t1
    v2 <- wait t2
    v3 <- readFinalValue stream3
    return $ show (v1, v2, v3)

simpleQueue :: TestTree
-- Push history should be available to the first consumer.
-- Pushing should block if the backing queue is full.
simpleQueue =
  testCase "simple-queue" [relfile|test/Vest/Prelude/Stream/simple-queue.gold|] $ do
    (writer, stream) <- newStream @(QueueBuffer_ 4) @Int
    writeStream writer 1
    writeStream writer 1
    writeStream writer 1
    threadDelay (sec 0.01)
    t1 <- asyncDetached $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    writeStream writer 1
    writeStream writer 1
    writeStream writer 1
    threadDelay (sec 0.01)
    t2 <- asyncDetached $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    replicateM_ 10 (writeStream writer 1)
    closeStream writer
    threadDelay (sec 0.01)
    l <- listFromStream stream
    sum1 <- wait t1
    sum2 <- wait t2
    return $ show (sum1, sum2, l)

multiQueue :: TestTree
multiQueue =
  testCase "multi-queue" [relfile|test/Vest/Prelude/Stream/multi-queue.gold|] $ do
    (writer, stream) <- newStream @(QueueBuffer_ 4) @Int
    writeStream writer 1
    writeStream writer 1
    writeStream writer 1
    threadDelay (sec 0.01)
    t1 <- asyncDetached $ foldStream (+) 0 stream
    threadDelay (sec 0.01)
    stream2 <- mapMStream (return . (* 2)) stream
    stream3 <- mapMStream (return . (* 3)) stream
    t2 <- asyncDetached $ foldStream (+) 0 stream2
    threadDelay (sec 0.01)
    t3 <- asyncDetached $ foldStream (+) 0 stream3
    threadDelay (sec 0.01)
    writeStream writer 1
    writeStream writer 1
    writeStream writer 1
    closeStream writer
    threadDelay (sec 0.01)
    sum1 <- wait t1
    sum2 <- wait t2
    sum3 <- wait t3
    return $ show (sum1, sum2, sum3)

toFromList :: TestTree
toFromList =
  testCase "to/from list" [relfile|test/Vest/Prelude/Stream/to-from-list.gold|] $
  streamFromList @QueueBuffer @Int [1, 2, 3] >>= listFromStream >>- show

takeStreamTest :: TestTree
takeStreamTest =
  testCase "take" [relfile|test/Vest/Prelude/Stream/take.gold|] $ do
    s <- streamFromList @QueueBuffer @Int [1, 2, 3]
    head <- takeStream 1 s >>= listFromStream
    tail <- listFromStream s
    return $ show (head, tail)

uncaughtExceptionTest :: TestTree
uncaughtExceptionTest =
  testCase
    "uncaught-exception"
    [relfile|test/Vest/Prelude/Stream/uncaught-exception.gold|] $
  catchAsync
    (do badThread <-
          asyncDetached $ do
            s <- streamFromList @QueueBuffer @Int [1, 2, 3]
            consumeStream (\_ -> throw BugException) s
        wait badThread
        return "Did not throw.")
    (\(ex :: SomeAsyncException) -> return $ show ex)

test_stream :: TestTree
test_stream =
  testGroup
    "Stream"
    [ simpleValue
    , multiValue
    , simpleQueue
    , multiQueue
    , toFromList
    , takeStreamTest
    , uncaughtExceptionTest
    ]
