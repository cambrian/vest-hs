import qualified Bridge
import qualified Data.List
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

echoTest :: Spec
echoTest =
  around (Bridge.with Bridge.localConfig) $
  context "when running simple echo RPC" $
  it "returns the original output" $ \b -> do
    let route = Route "echo"
        xs = [1, 2, 3]
    Bridge.serveRPC b route (\() -> return xs)
    result <- Bridge.callRPC @() @[Int] b route ()
    result `shouldBe` xs

echoTest' :: Spec
echoTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when running simple echo RPC" $
  it "returns the original output" $ \b -> do
    let route = Route "echo"
        xs = [1, 2, 3]
    Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
    results <- Bridge.callRPC' @() @Int b route ()
    Streamly.toList results `shouldReturn` xs

multipleFnTest :: Spec
multipleFnTest =
  around (Bridge.with Bridge.localConfig) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \b -> do
    let route = Route "echoInts"
        routeCh = Route "echoChars"
        xs = [1, 2, 3]
        chars = ['a', 'b', 'c']
    Bridge.serveRPC b route (\() -> return xs)
    Bridge.serveRPC b routeCh (\() -> return chars)
    result <- Bridge.callRPC @() @[Int] b route ()
    resultCh <- Bridge.callRPC @() @[Char] b routeCh ()
    result `shouldBe` xs
    resultCh `shouldBe` chars

multipleFnTest' :: Spec
multipleFnTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \b -> do
    let route = Route "echoInts"
        routeCh = Route "echoChars"
        xs = [1, 2, 3]
        chars = ['a', 'b', 'c']
    Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
    Bridge.serveRPC' b routeCh (\() -> return $ Streamly.fromList chars)
    results <- Bridge.callRPC' @() @Int b route ()
    resultsCh <- Bridge.callRPC' @() @Char b routeCh ()
    Streamly.toList results `shouldReturn` xs
    Streamly.toList resultsCh `shouldReturn` chars

multipleConsumeTest' :: Spec
multipleConsumeTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when running echo with multiple stream consumers" $
  it "returns the original output for both" $ \b -> do
    let route = Route "echo"
        xs = [1, 2, 3]
    Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
    results <- Bridge.callRPC' @() @Int b route ()
    Streamly.toList results `shouldReturn` xs
    Streamly.toList results `shouldReturn` xs

concurrentTest' :: Spec
concurrentTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when running RPCs with concurrent callers" $
  it "returns the correct output for both" $ \b -> do
    let route = Route "concurrent"
    Bridge.serveRPC'
      b
      route
      (\(x :: Int) ->
         return $
         Streamly.repeatM (threadDelay (sec 0.1) >> return x) & Streamly.take 3)
    results1 <- Bridge.callRPC' @Int @Int b route 1
    results2 <- Bridge.callRPC' @Int @Int b route 2
    resultList1 <- Streamly.toList results1
    resultList2 <- Streamly.toList results2
    resultList1 `shouldSatisfy` Data.List.all (== 1)
    resultList2 `shouldSatisfy` Data.List.all (== 2)

timeoutTest :: Spec
timeoutTest =
  around (Bridge.with Bridge.localConfig) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \b -> do
    let route = Route "echo"
        xs = [1, 2, 3] :: [Int] -- Annotation to shut up GHC.
    Bridge.serveRPC b route (\() -> threadDelay (sec 0.2) >> return xs)
    Bridge.callRPCTimeout @() @[Int] (sec 0.1) b route () `shouldThrow`
      (== Timeout (sec 0.1))

timeoutTest' :: Spec
timeoutTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \b -> do
    let route = Route "echo"
        xs = [1, 2, 3] :: [Int]
    Bridge.serveRPC'
      b
      route
      (\() ->
         return $
         Streamly.fromList xs &
         Streamly.mapM (\x -> threadDelay (sec 0.2) >> return x))
    (do results <- Bridge.callRPCTimeout' @() @Int (sec 0.1) b route ()
        -- Have to coerce results to actually reach the timeout.
        resultsList <- Streamly.toList results
        print resultsList) `shouldThrow`
      (== Timeout (sec 0.1))

pubSubTest' :: Spec
pubSubTest' =
  around (Bridge.with Bridge.localConfig) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \b -> do
    let route = Route "increment"
    async $ do
      let f s = do
            threadDelay (sec 0.05)
            return $
              if s < 5
                then Just (s, s + 1 :: Int)
                else Nothing
      Bridge.publish' b route (Streamly.unfoldrM f 0)
    (id, results) <- Bridge.subscribe' @Int b route
    async $ do
      threadDelay (sec 0.5)
      Bridge.unsubscribe' b id
    Streamly.toList results `shouldReturn` [0, 1, 2, 3, 4]

main :: IO ()
main = do
  return ()
  hspec $ do
    describe "direct RPC bridge" $ echoTest >> multipleFnTest >> timeoutTest
    describe "streaming RPC bridge" $
      echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest' >>
      timeoutTest'
    describe "publish/subscribe bridge" $ pubSubTest'
