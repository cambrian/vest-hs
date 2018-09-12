import qualified Bridge
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

localBridgeConfig :: Bridge.Config
localBridgeConfig =
  Bridge.Config
    { hostname = "localhost"
    , virtualHost = "/"
    , username = "guest"
    , password = "guest"
    }

withBridge :: Bridge.Config -> (Bridge.T -> IO ()) -> IO ()
withBridge config = bracket (Bridge.make config) Bridge.kill

killTest :: Spec
killTest = do
  context "when registering RPCs after server was killed" $ do
    it "correctly throws an exception" $ do
      bridge <- Bridge.make localBridgeConfig
      Bridge.kill bridge
      (Bridge.serveRPC bridge (Route "unit") (\() -> return ())) `shouldThrow`
        (== Bridge.DeadBridge)
      (Bridge.serveRPC'
         bridge
         (Route "nil")
         (\() -> return (Streamly.nil :: Streamly.Serial ()))) `shouldThrow`
        (== Bridge.DeadBridge)

echoTest :: Spec
echoTest = do
  around (withBridge localBridgeConfig) $ do
    context "when running simple echo RPC" $ do
      it "returns the original output" $ \b -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        Bridge.serveRPC b route (\() -> return xs)
        result <- Bridge.callRPC @() @[Int] b route ()
        result `shouldBe` xs

echoTest' :: Spec
echoTest' = do
  around (withBridge localBridgeConfig) $ do
    context "when running simple echo RPC" $ do
      it "returns the original output" $ \b -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
        results <- Bridge.callRPC' @() @Int b route ()
        Streamly.toList results `shouldReturn` xs

multipleFnTest :: Spec
multipleFnTest = do
  around (withBridge localBridgeConfig) $ do
    context "when running multiple echo RPCs" $ do
      it "returns the original output for both" $ \b -> do
        let (route, xs) = (Route "echoInts", [1, 2, 3] :: [Int])
        let (routeCh, chars) = (Route "echoChars", ['a', 'b', 'c'])
        Bridge.serveRPC b route (\() -> return xs)
        Bridge.serveRPC b routeCh (\() -> return chars)
        result <- Bridge.callRPC @() @[Int] b route ()
        resultCh <- Bridge.callRPC @() @[Char] b routeCh ()
        result `shouldBe` xs
        resultCh `shouldBe` chars

multipleFnTest' :: Spec
multipleFnTest' = do
  around (withBridge localBridgeConfig) $ do
    context "when running multiple echo RPCs" $ do
      it "returns the original output for both" $ \b -> do
        let (route, xs) = (Route "echoInts", [1, 2, 3] :: [Int])
        let (routeCh, chars) = (Route "echoChars", ['a', 'b', 'c'])
        Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
        Bridge.serveRPC' b routeCh (\() -> return $ Streamly.fromList chars)
        results <- Bridge.callRPC' @() @Int b route ()
        resultsCh <- Bridge.callRPC' @() @Char b routeCh ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList resultsCh `shouldReturn` chars

multipleConsumeTest' :: Spec
multipleConsumeTest' = do
  around (withBridge localBridgeConfig) $ do
    context "when running echo with multiple stream consumers" $ do
      it "returns the original output for both" $ \b -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        Bridge.serveRPC' b route (\() -> return $ Streamly.fromList xs)
        results <- Bridge.callRPC' @() @Int b route ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList results `shouldReturn` xs

concurrentTest' :: Spec
concurrentTest' = do
  around (withBridge localBridgeConfig) $ do
    context "when running RPCs with concurrent callers" $ do
      it "returns the correct output for both" $ \b -> do
        let route = Route "concurrent"
        Bridge.serveRPC'
          b
          route
          (\(x :: Int) ->
             return $
             Streamly.repeatM (threadDelay 10000 >> return x) & Streamly.take 3)
        results1 <- Bridge.callRPC' @Int @Int b route 1
        results2 <- Bridge.callRPC' @Int @Int b route 2
        resultList1 <- Streamly.toList results1
        resultList2 <- Streamly.toList results2
        resultList1 `shouldSatisfy` Data.List.all (\x -> x == 1)
        resultList2 `shouldSatisfy` Data.List.all (\x -> x == 2)

timeoutTest :: Spec
timeoutTest = do
  around (withBridge localBridgeConfig) $ do
    context "when running RPCs that take too long" $ do
      it "forces callers to time out" $ \b -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        Bridge.serveRPC b route (\() -> threadDelay 300000 >> return xs)
        (Bridge.callRPCTimeout @() @[Int] (secondsToDiffTime 0.1) b route ()) `shouldThrow`
          (== Timeout 100000)

timeoutTest' :: Spec
timeoutTest' = do
  around (withBridge localBridgeConfig) $ do
    context "when running RPCs that take too long" $ do
      it "forces callers to time out" $ \b -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        Bridge.serveRPC'
          b
          route
          (\() ->
             return $
             Streamly.fromList xs &
             Streamly.mapM (\x -> threadDelay (50000 * x) >> return x))
        (do results <-
              Bridge.callRPCTimeout' @() @Int (secondsToDiffTime 0.1) b route ()
            -- Have to coerce results to actually reach the timeout.
            resultsList <- Streamly.toList results
            print resultsList) `shouldThrow`
          (== Timeout 100000)

main :: IO ()
main = do
  hspec $ do
    describe "bridge server library" $ killTest
    describe "direct RPC bridge" $ echoTest >> multipleFnTest >> timeoutTest
    describe "streaming RPC bridge" $
      echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest' >>
      timeoutTest'
