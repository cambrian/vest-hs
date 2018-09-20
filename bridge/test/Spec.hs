-- Should be disabled to destructure makeX calls without manual type signatures.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Bridge
import qualified Butler
import Butler (DirectEndpoint, StreamingEndpoint, Topic)
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

type RpcApi
   = DirectEndpoint "echo" Text Text
     :<|> DirectEndpoint "echoInts" [Int] [Int]
     :<|> StreamingEndpoint "echoInts'" [Int] Int
     :<|> DirectEndpoint "echoChars" [Char] [Char]
     :<|> StreamingEndpoint "echoChars'" [Char] Char
     :<|> DirectEndpoint "echoTimeout" [Int] [Int]
     :<|> StreamingEndpoint "echoTimeout'" [Int] Int
     :<|> StreamingEndpoint "echoDelay'" Int Int

echo :: Text -> IO Text
echo = return . identity

echoInts :: [Int] -> IO [Int]
echoInts = return . identity

echoInts' :: [Int] -> IO (Streamly.Serial Int)
echoInts' = return . Streamly.fromList

echoChars :: [Char] -> IO [Char]
echoChars = return . identity

echoChars' :: [Char] -> IO (Streamly.Serial Char)
echoChars' = return . Streamly.fromList

echoTimeout :: [Int] -> IO [Int]
echoTimeout xs = threadDelay (sec 0.2) >> return xs

echoTimeout' :: [Int] -> IO (Streamly.Serial Int)
echoTimeout' xs =
  return $
  Streamly.fromList xs & Streamly.mapM (\x -> threadDelay (sec 0.2) >> return x)

echoDelay' :: Int -> IO (Streamly.Serial Int)
echoDelay' x =
  return $
  Streamly.repeatM (threadDelay (sec 0.1) >> return x) & Streamly.take 3

handlers :: Butler.Handlers RpcApi
handlers =
  echo :<|> echoInts :<|> echoInts' :<|> echoChars :<|> echoChars' :<|>
  echoTimeout :<|>
  echoTimeout' :<|>
  echoDelay'

-- callEcho :<|> callEchoInts :<|> callEchoChars :<|> callEchoTimeout =
--   Butler.makeClient proxyDirectAPI Bridge.callUntyped
-- callEchoInts' :<|> callEchoChars' :<|> callEchoDelay' :<|> callEchoTimeout' =
--   Butler.makeClient' proxyStreamingAPI Bridge.callUntyped'
type PubSubApi = Topic "increment" Int

-- subscribeIncrement =
--   Butler.makeSubscriber' proxyPublishAPI Bridge.subscribeUntyped'
increment :: Streamly.Serial Int
increment =
  let f s = do
        threadDelay (sec 0.05)
        return $
          if s < 5
            then Just (s, s + 1 :: Int)
            else Nothing
   in Streamly.unfoldrM f 0

-- withBridgeClientBindings ::
--      Bridge.Config
--   -> Proxy spec
--   -> (Butler.ClientBindings spec -> IO ())
--   -> IO ()
-- withBridgeClientBindings config spec action =
--   with
--     config
--     (\(bridge :: Bridge.T) -> do
--        Butler.serve (Proxy :: Proxy RpcApi) bridge handlers
--        let bindings = Butler.makeClient spec bridge
--        action bindings)
echoTest :: Spec
echoTest = do
  let withServer config action =
        with
          config
          (\(bridge :: Bridge.T) -> do
             Butler.serve (Proxy :: Proxy (RpcApi, Bridge.T)) bridge handlers
             let callEcho =
                   Butler.makeClient
                     (Proxy :: Proxy (DirectEndpoint "echo" Text Text, Bridge.T))
                     bridge
             action callEcho)
   in around (withServer Bridge.localConfig) $
      context "when running simple echo RPC" $
      it "returns the original output" $ \callEcho -> do
        result <- callEcho (sec 1) "test"
        result `shouldBe` "test"

-- echoTest' :: Spec
-- echoTest' =
--   around (Bridge.with config) $
--   context "when running simple echoInts RPC" $
--   it "returns the original output" $ \b -> do
--     results <- callEchoInts' Bridge.defaultTimeout b [1, 2, 3]
--     Streamly.toList results `shouldReturn` [1, 2, 3]
-- multipleFnTest :: Spec
-- multipleFnTest =
--   around (Bridge.with config) $
--   context "when running multiple echo RPCs" $
--   it "returns the original output for both" $ \b -> do
--     resultInts <- callEchoInts Bridge.defaultTimeout b [1, 2, 3]
--     resultChars <- callEchoChars Bridge.defaultTimeout b ['a', 'b', 'c']
--     resultInts `shouldBe` [1, 2, 3]
--     resultChars `shouldBe` ['a', 'b', 'c']
-- multipleFnTest' :: Spec
-- multipleFnTest' =
--   around (Bridge.with config) $
--   context "when running multiple echo RPCs" $
--   it "returns the original output for both" $ \b -> do
--     resultsInt <- callEchoInts' Bridge.defaultTimeout b [1, 2, 3]
--     resultsChar <- callEchoChars' Bridge.defaultTimeout b ['a', 'b', 'c']
--     Streamly.toList resultsInt `shouldReturn` [1, 2, 3]
--     Streamly.toList resultsChar `shouldReturn` ['a', 'b', 'c']
-- multipleConsumeTest' :: Spec
-- multipleConsumeTest' =
--   around (Bridge.with config) $
--   context "when running echo with multiple stream consumers" $
--   it "returns the original output for both" $ \b -> do
--     results <- callEchoInts' Bridge.defaultTimeout b [1, 2, 3]
--     Streamly.toList results `shouldReturn` [1, 2, 3]
--     Streamly.toList results `shouldReturn` [1, 2, 3]
-- concurrentTest' :: Spec
-- concurrentTest' =
--   around (Bridge.with config) $
--   context "when running RPCs with concurrent callers" $
--   it "returns the correct output for both" $ \b -> do
--     results1 <- callEchoDelay' Bridge.defaultTimeout b 1
--     results2 <- callEchoDelay' Bridge.defaultTimeout b 2
--     resultList1 <- Streamly.toList results1
--     resultList2 <- Streamly.toList results2
--     resultList1 `shouldSatisfy` Data.List.all (== 1)
--     resultList2 `shouldSatisfy` Data.List.all (== 2)
-- timeoutTest :: Spec
-- timeoutTest =
--   around (Bridge.with config) $
--   context "when running RPCs that take too long" $
--   it "forces callers to time out" $ \b ->
--     callEchoTimeout (sec 0.1) b [1, 2, 3] `shouldThrow` (== Timeout (sec 0.1))
-- timeoutTest' :: Spec
-- timeoutTest' =
--   around (Bridge.with config) $
--   context "when running RPCs that take too long" $
--   it "forces callers to time out" $ \b ->
--     (do results <- callEchoTimeout' (sec 0.1) b [1, 2, 3]
--         resultsList <- Streamly.toList results
--         print resultsList) `shouldThrow`
--     (== Timeout (sec 0.1))
-- pubSubTest' :: Spec
-- pubSubTest' =
--   around (Bridge.with config) $
--   context "when publishing an incrementing stream" $
--   it "functions correctly on the subscribing end" $ \b -> do
--     (id, results) <- subscribeIncrement b
--     publishIncrement b increment
--     async $ do
--       threadDelay (sec 0.5)
--       Bridge.unsubscribe' b id
--     Streamly.toList results `shouldReturn` [0, 1, 2, 3, 4]
main :: IO ()
main = hspec $ do describe "direct RPC bridge" $ echoTest
    -- describe "direct RPC bridge" $ echoTest >> multipleFnTest >> timeoutTest
    -- describe "streaming RPC bridge" $
    --   echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest' >>
    --   timeoutTest'
    -- describe "publish/subscribe bridge" pubSubTest'
