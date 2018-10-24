module Vest.Prelude.Stream.ObserveFromIndexTest
  ( test_observe_from_index
  ) where

import qualified Stream
import Test
import Vest

instance Indexable Word where
  type IndexOf Word = Word
  index = identity
  minIndex = 0

materialize :: Word -> IO Word
materialize x = threadDelay (ms 20) >> return x

observeFromIndexTest :: TestTree
observeFromIndexTest =
  testCase "ObserveFromIndex" "test/Vest/Prelude/Stream/observe-from-index.gold" $ do
    let original = Stream.fromList [4, 5]
    result <- newTVarIO []
    observeFromIndex
      original
      materialize
      (\res -> atomically $ modifyTVar result (<> [res]))
      2 -- Produces indices [2, 3, 4, 5].
    show <$> readTVarIO result

test_observe_from_index :: TestTree
test_observe_from_index = testGroup "ObserveFromIndex" [observeFromIndexTest]
