import qualified Data.List
import qualified SocketServer
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

localTestConfig :: Int -> SocketServer.SocketClientConfig
-- For some sus reason, this won't connect with "localhost" as the URI.
localTestConfig port = SocketServer.makeClientConfig "127.0.0.1" port "/"

withServerOnPort :: Int -> (SocketServer.T -> IO ()) -> IO ()
withServerOnPort port =
  bracket (SocketServer.makeServer port) SocketServer.killServer

echoTest' :: Spec
echoTest' = do
  around (withServerOnPort 3000) $ do
    context "when running simple echo RPC" $ do
      it "returns the original output" $ \s -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        results <-
          SocketServer.callRPC' @() @Int (localTestConfig 3000) route ()
        Streamly.toList results `shouldReturn` xs

multipleFnTest' :: Spec
multipleFnTest' = do
  around (withServerOnPort 3000) $ do
    context "when running multiple echo RPCs" $ do
      it "returns the original output for both" $ \s -> do
        let (route, xs) = (Route "echoInts", [1, 2, 3] :: [Int])
        let (routeCh, chars) = (Route "echoChars", ['a', 'b', 'c'])
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        SocketServer.serveRPC'
          s
          routeCh
          (\() -> return $ Streamly.fromList chars)
        results <-
          SocketServer.callRPC' @() @Int (localTestConfig 3000) route ()
        resultsCh <-
          SocketServer.callRPC' @() @Char (localTestConfig 3000) routeCh ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList resultsCh `shouldReturn` chars

multipleConsumeTest' :: Spec
multipleConsumeTest' = do
  around (withServerOnPort 3000) $ do
    context "when running echo with multiple stream consumers" $ do
      it "returns the original output for both" $ \s -> do
        let (route, xs) = (Route "echo", [1, 2, 3] :: [Int])
        SocketServer.serveRPC' s route (\() -> return $ Streamly.fromList xs)
        results <-
          SocketServer.callRPC' @() @Int (localTestConfig 3000) route ()
        Streamly.toList results `shouldReturn` xs
        Streamly.toList results `shouldReturn` xs

concurrentTest' :: Spec
concurrentTest' = do
  around (withServerOnPort 3000) $ do
    context "when running RPCs with concurrent callers" $ do
      it "returns the correct output for both" $ \s -> do
        let route = Route "concurrent"
        SocketServer.serveRPC'
          s
          route
          (\(x :: Int) ->
             return $
             Streamly.repeatM (threadDelay 100000 >> return x) & Streamly.take 3)
        results1 <-
          SocketServer.callRPC' @Int @Int (localTestConfig 3000) route 1
        results2 <-
          SocketServer.callRPC' @Int @Int (localTestConfig 3000) route 2
        resultList1 <- Streamly.toList results1
        resultList2 <- Streamly.toList results2
        resultList1 `shouldSatisfy` Data.List.all (\x -> x == 1)
        resultList2 `shouldSatisfy` Data.List.all (\x -> x == 2)

main :: IO ()
main = do
  return ()
  hspec $ do
    describe "streaming socket server" $
      echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest'
