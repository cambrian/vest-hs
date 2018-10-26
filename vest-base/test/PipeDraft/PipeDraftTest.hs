module PipeDraft.PipeDraftTest
  ( test_pipe
  ) where

import PipeDraft
import Test
import Vest

toListAsync :: Pipe a -> IO (Async [a])
toListAsync pipe =
  async $ do
    resultListVar <- newTVarIO []
    consumePipe (\x -> atomically $ modifyTVar' resultListVar (++ [x])) pipe
    readTVarIO resultListVar

-- The test below creates a tree of consumers that looks something like this:
--
--           startPipe -------------------------------
--         /          \              \                 \
--    timesTwo      toListAsync    toListAsync     postCloseList
--       |     \                   (late bind)
--       |    toListAsync
--       |
--  timesTwoNegative
--       |
--   toListAsync
--
-- What each consumer sees is either
-- (1) everything (if it was first to bind to its parent)
--   OR
-- (2) some things (depending on when values were pushed and when it was bound).
pipeTest :: TestTree
pipeTest =
  testCase "pipeTest" "test/PipeDraft/pipe-test.gold" $ do
    (push, close, startPipe) <- newPipe @Int unbounded
    startList <- toListAsync startPipe
    timesTwo <- mapPipe (return . (* 2)) startPipe
    timesTwoList <- toListAsync timesTwo
    timesTwoNegative <- mapPipe (return . (-) 0) timesTwo
    timesTwoNegativeList <- toListAsync timesTwoNegative
    mapM_ push [1, 2, 3]
    threadDelay (ms 30)
    startListLateBind <- toListAsync startPipe
    threadDelay (ms 30)
    push 4
    close
    threadDelay (ms 30)
    mapM_ push [1, 2, 3]
    postClose <- mapPipe (return . (* 2)) startPipe
    postCloseList <- toListAsync postClose
    mapM
      wait
      [ startList -- [1, 2, 3, 4]
      , timesTwoList -- [2, 4, 6, 8]
      , timesTwoNegativeList -- [-2, -4, -6, -8]
      , startListLateBind -- [4]
      , postCloseList -- []
      ] >>-
      show

test_pipe :: TestTree
test_pipe = testGroup "Pipe" [pipeTest]
