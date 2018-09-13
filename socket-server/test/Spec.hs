import qualified Data.List
import qualified SocketServer
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

clientConfig :: SocketServer.SocketClientConfig
clientConfig = SocketServer.localClientConfig

echoTest :: Spec
echoTest = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running simple echo RPC" $ do
      it "returns the original output" $ \s -> do
        let route = Route "echo"
            xs = [1, 2, 3]
        SocketServer.serveRPC s route (\() -> return xs)
        result <- SocketServer.callRPC @() @[Int] clientConfig route ()
        result `shouldBe` xs

echoTest' :: Spec
echoTest' = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running simple echo RPC" $ do
      it "returns the original output" $ \s -> do
        let route = Route "echo"
            xs = [1, 2, 3]
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        results <- SocketServer.callRPC' @() @Int clientConfig route ()
        Streamly.toList results `shouldReturn` xs

multipleFnTest :: Spec
multipleFnTest = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running multiple echo RPCs" $ do
      it "returns the original output for both" $ \s -> do
        let route = Route "echoInts"
            routeCh = Route "echoChars"
            xs = [1, 2, 3]
            chars = ['a', 'b', 'c']
        SocketServer.serveRPC s route (\() -> return xs)
        SocketServer.serveRPC s routeCh (\() -> return chars)
        result <- SocketServer.callRPC @() @[Int] clientConfig route ()
        resultCh <- SocketServer.callRPC @() @[Char] clientConfig routeCh ()
        result `shouldBe` xs
        resultCh `shouldBe` chars

multipleFnTest' :: Spec
multipleFnTest' = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running multiple echo RPCs" $ do
      it "returns the original output for both" $ \s -> do
        let route = Route "echoInts"
            routeCh = Route "echoChars"
            xs = [1, 2, 3]
            chars = ['a', 'b', 'c']
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        SocketServer.serveRPC'
          s
          routeCh
          (\() -> return $ Streamly.fromList chars)
        results <- SocketServer.callRPC' @() @Int clientConfig route ()
        resultsCh <- SocketServer.callRPC' @() @Char clientConfig routeCh ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList resultsCh `shouldReturn` chars

multipleConsumeTest' :: Spec
multipleConsumeTest' = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running echo with multiple stream consumers" $ do
      it "returns the original output for both" $ \s -> do
        let route = Route "echo"
            xs = [1, 2, 3]
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        results <- SocketServer.callRPC' @() @Int clientConfig route ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList results `shouldReturn` xs

concurrentTest' :: Spec
concurrentTest' = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running RPCs with concurrent callers" $ do
      it "returns the correct output for both" $ \s -> do
        let route = Route "concurrent"
        SocketServer.serveRPC'
          s
          route
          (\(x :: Int) ->
             return $
             Streamly.repeatM (threadDelay (sec 0.1) >> return x) &
             Streamly.take 3)
        results1 <- SocketServer.callRPC' @Int @Int clientConfig route 1
        results2 <- SocketServer.callRPC' @Int @Int clientConfig route 2
        resultList1 <- Streamly.toList results1
        resultList2 <- Streamly.toList results2
        resultList1 `shouldSatisfy` Data.List.all (\x -> x == 1)
        resultList2 `shouldSatisfy` Data.List.all (\x -> x == 2)

timeoutTest :: Spec
timeoutTest = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running RPCs that take too long" $ do
      it "forces callers to time out" $ \s -> do
        let route = Route "echo"
            xs = [1, 2, 3] :: [Int] -- Annotation to shut up GHC.
        SocketServer.serveRPC
          s
          route
          (\() -> threadDelay (sec 0.2) >> return xs)
        (SocketServer.callRPCTimeout @() @[Int] (sec 0.1) clientConfig route ()) `shouldThrow`
          (== Timeout (sec 0.1))

timeoutTest' :: Spec
timeoutTest' = do
  around (SocketServer.with SocketServer.localConfig) $ do
    context "when running RPCs that take too long" $ do
      it "forces callers to time out" $ \s -> do
        let route = Route "echo"
            xs = [1, 2, 3] :: [Int]
        SocketServer.serveRPC'
          s
          route
          (\() ->
             return $
             Streamly.fromList xs &
             Streamly.mapM (\x -> threadDelay (sec 0.2) >> return x))
        (do results <-
              SocketServer.callRPCTimeout'
                @()
                @Int
                (sec 0.1)
                clientConfig
                route
                ()
            -- Have to coerce results to actually reach the timeout.
            resultsList <- Streamly.toList results
            print resultsList) `shouldThrow`
          (== Timeout (sec 0.1))

main :: IO ()
main = do
  hspec $ do
    describe "direct RPC socket server" $
      echoTest >> multipleFnTest >> timeoutTest
    describe "streaming RPC socket server" $
      echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest' >>
      timeoutTest'
