module Vest.Prelude.Stream.ObserveFromIndexTest
  ( test_observe_from_index
  ) where

import qualified Stream
import Test
import Vest

data WrappedInt = WrappedInt
  { wrapped :: Int
  } deriving (Show)

instance Indexable WrappedInt where
  type IndexOf WrappedInt = Int
  index = wrapped

wrap :: Int -> WrappedInt
wrap x = WrappedInt {wrapped = x}

materialize :: Int -> IO WrappedInt
materialize x = threadDelay (ms 20) >> return (WrappedInt {wrapped = x})

observeFromIndexTest :: TestTree
observeFromIndexTest =
  testCase "ObserveFromIndex" "test/Vest/Prelude/Stream/observe-from-index.gold" $ do
    let original = Stream.fromList [wrap 4, wrap 5]
    result <- newTVarIO []
    observeFromIndex
      original
      materialize
      (\res -> atomically $ modifyTVar result (<> [res]))
      2 -- Produces indices [2, 3, 4, 5].
    show <$> readTVarIO result

test_observe_from_index :: TestTree
test_observe_from_index = testGroup "ObserveFromIndex" [observeFromIndexTest]
