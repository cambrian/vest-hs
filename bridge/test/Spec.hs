import Bridge
import qualified Bridge.Transports.Amqp as Amqp

import Bridge.Rpc.Prelude ()
import qualified Bridge.Transports.WebSocket as WebSocket
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

type EchoIntsDirectEndpoint
   = Endpoint 'NoAuth "echoIntsDirect" [Int] ('Direct [Int])

type EchoTextsDirectEndpoint
   = Endpoint 'NoAuth "echoTextDirect" [Text] ('Direct [Text])

type EchoIntsStreamingEndpoint
   = Endpoint ('Auth TokenAuth) "echoIntsStreaming" [Int] ('Streaming Int)

type EchoTextsStreamingEndpoint
   = Endpoint ('Auth TokenAuth) "echoTextsStreaming" [Text] ('Streaming Text)

type RpcApi
   = EchoIntsDirectEndpoint
     :<|> EchoTextsDirectEndpoint
     :<|> EchoIntsStreamingEndpoint
     :<|> EchoTextsStreamingEndpoint

echoDirect :: a -> IO a
echoDirect x = threadDelay (sec 0.01) >> return x

echoStreaming :: Claims auth -> [a] -> IO (Streamly.Serial a)
echoStreaming _ xs =
  return $ Streamly.fromList xs & Streamly.mapM (<$ threadDelay (sec 0.01))

handlers :: Handlers RpcApi
handlers = echoDirect :<|> echoDirect :<|> echoStreaming :<|> echoStreaming

withRpcClient ::
     forall spec transport. (Resource transport, Client spec transport)
  => ResourceConfig transport
  -> Proxy (spec, transport)
  -> (ClientBindings spec -> IO ())
  -> IO ()
withRpcClient config _ action =
  with
    config
    (\transport -> do
       serve handlers (Proxy :: Proxy (RpcApi, transport)) transport
       threadDelay (sec 0.01) -- Wait for servers to initialize and avoid races.
       action $ makeClient (Proxy :: Proxy (spec, transport)) transport)

increment :: Streamly.Serial Int
increment =
  let f s = do
        threadDelay (sec 0.01)
        return $
          if s < 5
            then Just (s, s + 1 :: Int)
            else Nothing
   in Streamly.unfoldrM f 0

type IncrementTopic = Topic 'Haskell "increment" Int

type PubSubApi = IncrementTopic

streams :: Streams PubSubApi
streams = increment

withSubscribed ::
     forall spec transport. (Resource transport, Subscriber spec transport)
  => ResourceConfig transport
  -> Proxy (spec, transport)
  -> (SubscriberBindings spec -> IO ())
  -> IO ()
withSubscribed config _ action =
  with
    config
    (\transport -> do
       subscribed <- subscribe (Proxy :: Proxy (spec, transport)) transport
       publish streams (Proxy :: Proxy (PubSubApi, transport)) transport
       action subscribed)

tokenAuthJSON :: Headers
tokenAuthJSON = Headers {format = JSON, token = Just $ Tagged ""}

singleDirectTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
singleDirectTest config =
  around (withRpcClient config (Proxy :: Proxy (EchoIntsDirectEndpoint, a))) $
  context "with a single direct RPC" $ do
    it "makes a single call" $ \call -> do
      result <- call (sec 1) defaultHeaders [1, 2, 3]
      result `shouldBe` [1, 2, 3]
    it "times out for a single call" $ \call ->
      call (sec 0) defaultHeaders [1, 2, 3] `shouldThrow`
      (== TimeoutException (sec 0))

singleStreamingTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
singleStreamingTest config =
  around (withRpcClient config (Proxy :: Proxy (EchoIntsStreamingEndpoint, a))) $
  context "with a single streaming RPC" $ do
    it "receives the last result for a single call" $ \call -> do
      results <- call (sec 1) tokenAuthJSON [1, 2, 3]
      (Streamly.toList results >>- elem 3) `shouldReturn` True
    it "sees the last item for every fanout" $ \call -> do
      results <- call (sec 1) tokenAuthJSON [1, 2, 3]
      (Streamly.toList results >>- elem 3) `shouldReturn` True
      (Streamly.toList results >>- elem 3) `shouldReturn` True
    -- Note: The timeout is not identified properly if the results are not forced.
    it "times out for a single call" $ \call ->
      (do results <- call (sec 0) tokenAuthJSON [1, 2, 3]
          resultList <- Streamly.toList results
          print resultList) `shouldThrow`
      (== TimeoutException (sec 0))

multipleDirectTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
multipleDirectTest config =
  around
    (withRpcClient
       config
       (Proxy :: Proxy ( EchoIntsDirectEndpoint
                         :<|> EchoTextsDirectEndpoint
                       , a))) $
  context "when running multiple direct RPCs" $
  it "makes one call to each" $ \(echoInts :<|> echoTexts) -> do
    resultInts <- echoInts (sec 1) defaultHeaders [1, 2, 3]
    resultTexts <- echoTexts (sec 1) defaultHeaders ["a", "b", "c"]
    resultInts `shouldBe` [1, 2, 3]
    resultTexts `shouldBe` ["a", "b", "c"]

multipleStreamingTest ::
     (Resource a, RpcTransport a) => ResourceConfig a -> Spec
multipleStreamingTest config =
  around
    (withRpcClient
       config
       (Proxy :: Proxy ( EchoIntsStreamingEndpoint
                         :<|> EchoTextsStreamingEndpoint
                       , a))) $
  context "when running multiple streaming RPCs" $ do
    it "sees the last item for each call" $ \(echoInts :<|> echoTexts) -> do
      resultsInt <- echoInts (sec 1) tokenAuthJSON [1, 2, 3]
      resultsText <- echoTexts (sec 1) tokenAuthJSON ["a", "b", "c"]
      (Streamly.toList resultsInt >>- elem 3) `shouldReturn` True
      (Streamly.toList resultsText >>- elem "c") `shouldReturn` True
    it "handles concurrent calls" $ \(echoInts :<|> _) -> do
      results1 <- echoInts (sec 1) tokenAuthJSON $ replicate 3 1
      results2 <- echoInts (sec 1) tokenAuthJSON $ replicate 3 2
      resultList1 <- Streamly.toList results1
      resultList2 <- Streamly.toList results2
      resultList1 `shouldSatisfy` Data.List.all (== 1)
      resultList2 `shouldSatisfy` Data.List.all (== 2)

pubSubTest' :: (Resource a, PubSubTransport a) => ResourceConfig a -> Spec
pubSubTest' config =
  around (withSubscribed config (Proxy :: Proxy (IncrementTopic, a))) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Streamly.toList (Streamly.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

directTests :: (Resource a, RpcTransport a) => [ResourceConfig a -> Spec]
directTests = [singleDirectTest, multipleDirectTest]

streamingTests :: (Resource a, RpcTransport a) => [ResourceConfig a -> Spec]
streamingTests = [singleStreamingTest, multipleStreamingTest]

pubSubTests :: (Resource a, PubSubTransport a) => [ResourceConfig a -> Spec]
pubSubTests = [pubSubTest']

webSocketConfig :: WebSocket.Config
webSocketConfig =
  WebSocket.localConfig
    { WebSocket.servers =
        [ WebSocket.ServerInfo
            { uri = Tagged "127.0.0.1"
            , port = Tagged 3000
            , path = Tagged "/"
            , routes =
                map stringToText' . manySymbolVal $
                (Proxy :: Proxy (NubRoutes RpcApi))
            }
        ]
    }

main :: IO ()
main = do
  let amqpMake = ($ Amqp.localConfig)
      wsMake = ($ webSocketConfig)
  hspec $ do
    describe "AMQP bridge" $ do
      describe "Direct RPC" $ mapM_ amqpMake directTests
      describe "Streaming RPC" $ mapM_ amqpMake streamingTests
      describe "Pub/Sub" $ mapM_ amqpMake pubSubTests
    describe "WebSocket bridge" $ do
      describe "Direct RPC" $ mapM_ wsMake directTests
      describe "Streaming RPC" $ mapM_ wsMake streamingTests
