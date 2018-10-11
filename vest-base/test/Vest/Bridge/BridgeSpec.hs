module Vest.Bridge.BridgeSpec
  ( spec
  ) where

import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import Vest.Bridge
import Vest.Bridge.Rpc.Prelude ()
import qualified Vest.Bridge.Transports.Amqp as Amqp
import qualified Vest.Bridge.Transports.WebSocket as WebSocket
import Vest.Prelude

data DummyService = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasPubSubTransport Amqp.T T where
  pubSubTransport = amqp

type EchoIntsDirectEndpoint transport
   = Endpoint "Haskell" 'NoAuth T transport "echoIntsDirect" [Int] ('Direct [Int])

type EchoTextsDirectEndpoint transport
   = Endpoint "Haskell" 'NoAuth T transport "echoTextDirect" [Text] ('Direct [Text])

type EchoIntsStreamingEndpoint transport
   = Endpoint "Haskell" 'NoAuth T transport "echoIntsStreaming" [Int] ('Streaming Int)

type EchoTextsStreamingEndpoint transport
   = Endpoint "Haskell" 'NoAuth T transport "echoTextsStreaming" [Text] ('Streaming Text)

type TestRpcApi transport
   = EchoIntsDirectEndpoint transport
     :<|> EchoTextsDirectEndpoint transport
     :<|> EchoIntsStreamingEndpoint transport
     :<|> EchoTextsStreamingEndpoint transport

instance HasNamespace T where
  namespace = Tagged "test"

withT :: (T -> IO a) -> IO a
withT f =
  with webSocketTestConfig $ \webSocket ->
    with Amqp.localConfig (\amqp -> f $ T {amqp, webSocket})

echoDirect :: T -> a -> IO a
echoDirect _ x = threadDelay (sec 0.01) >> return x

echoStreaming :: T -> [a] -> IO (Streamly.Serial a)
echoStreaming _ xs =
  return $ Streamly.fromList xs & Streamly.mapM (<$ threadDelay (sec 0.01))

handlers :: Handlers (TestRpcApi Amqp.T)
handlers = echoDirect :<|> echoDirect :<|> echoStreaming :<|> echoStreaming

withRpcClient ::
     forall transport spec. (Server T (TestRpcApi transport), Client T spec)
  => Proxy spec
  -> (ClientBindings spec -> IO ())
  -> IO ()
withRpcClient _ f =
  withT $ \t -> do
    serve handlers t (Proxy :: Proxy (TestRpcApi transport))
    f $ makeClient t (Proxy :: Proxy spec)

increment :: Streamly.Serial Int
increment =
  let f s = do
        threadDelay (sec 0.01)
        return $
          if s < 5
            then Just (s, s + 1 :: Int)
            else Nothing
   in Streamly.unfoldrM f 0

type IncrementTopic transport = Topic "Haskell" T transport "increment" Int

type TestPubSubApi transport = IncrementTopic transport

streams :: Streams (TestPubSubApi Amqp.T)
streams = increment

withSubscribed ::
     forall transport spec.
     (Publisher T (TestPubSubApi transport), Subscriber T spec)
  => Proxy spec
  -> (SubscriberBindings spec -> IO ())
  -> IO ()
withSubscribed _ f =
  withT $ \t -> do
    subscribed <- subscribe t (Proxy :: Proxy spec)
    publish streams t (Proxy :: Proxy (TestPubSubApi transport))
    f subscribed

emptyRpcTest :: Spec
emptyRpcTest =
  around
    (\f ->
       withT $ \t -> do
         serve () t (Proxy :: Proxy ())
         f $ makeClient t (Proxy :: Proxy ())) $
  context "with empty handlers and API" $
  it "doesn't complain" $ \() -> True `shouldBe` True

emptyPubSubTest :: Spec
emptyPubSubTest =
  around
    (\f ->
       withT $ \t -> do
         subscribed <- subscribe t (Proxy :: Proxy ())
         publish () t (Proxy :: Proxy ())
         f subscribed) $
  context "with empty publish and subscribe" $
  it "doesn't complain" $ \() -> True `shouldBe` True

singleDirectTest ::
     forall transport. HasRpcTransport transport T
  => Spec
singleDirectTest =
  around
    (withRpcClient
       @transport
       (Proxy :: Proxy (EchoIntsDirectEndpoint transport))) $
  context "with a single direct RPC" $ do
    it "makes a single call" $ \call -> do
      result <- call (sec 1) [1, 2, 3]
      result `shouldBe` [1, 2, 3]
    it "times out for a single call" $ \call ->
      call (sec 0) [1, 2, 3] `shouldThrow` (== TimeoutException (sec 0))

singleStreamingTest ::
     forall transport. HasRpcTransport transport T
  => Spec
singleStreamingTest =
  around
    (withRpcClient
       @transport
       (Proxy :: Proxy (EchoIntsStreamingEndpoint transport))) $
  context "with a single streaming RPC" $ do
    it "receives the last result for a single call" $ \call -> do
      results <- call (sec 1) [1, 2, 3]
      (Streamly.toList results >>- elem 3) `shouldReturn` True
    it "sees the last item for every fanout" $ \call -> do
      results <- call (sec 1) [1, 2, 3]
      (Streamly.toList results >>- elem 3) `shouldReturn` True
      (Streamly.toList results >>- elem 3) `shouldReturn` True
      -- Note: The timeout is not identified properly if the results are not forced.
    it "times out for a single call" $ \call ->
      (do results <- call (sec 0) [1, 2, 3]
          resultList <- Streamly.toList results
          print resultList) `shouldThrow`
      (== TimeoutException (sec 0))

multipleDirectTest ::
     forall transport. HasRpcTransport transport T
  => Spec
multipleDirectTest =
  around
    (withRpcClient
       @transport
       (Proxy :: Proxy (EchoIntsDirectEndpoint transport
                        :<|> EchoTextsDirectEndpoint transport))) $
  context "when running multiple direct RPCs" $
  it "makes one call to each" $ \(echoInts :<|> echoTexts) -> do
    resultInts <- echoInts (sec 1) [1, 2, 3]
    resultTexts <- echoTexts (sec 1) ["a", "b", "c"]
    resultInts `shouldBe` [1, 2, 3]
    resultTexts `shouldBe` ["a", "b", "c"]

multipleStreamingTest ::
     forall transport. HasRpcTransport transport T
  => Spec
multipleStreamingTest =
  around
    (withRpcClient
       @transport
       (Proxy :: Proxy (EchoIntsStreamingEndpoint transport
                        :<|> EchoTextsStreamingEndpoint transport))) $
  context "when running multiple streaming RPCs" $ do
    it "sees the last item for each call" $ \(echoInts :<|> echoTexts) -> do
      resultsInt <- echoInts (sec 1) [1, 2, 3]
      resultsText <- echoTexts (sec 1) ["a", "b", "c"]
      (Streamly.toList resultsInt >>- elem 3) `shouldReturn` True
      (Streamly.toList resultsText >>- elem "c") `shouldReturn` True
    it "handles concurrent calls" $ \(echoInts :<|> _) -> do
      results1 <- echoInts (sec 1) $ replicate 3 1
      results2 <- echoInts (sec 1) $ replicate 3 2
      resultList1 <- Streamly.toList results1
      resultList2 <- Streamly.toList results2
      resultList1 `shouldSatisfy` Data.List.all (== 1)
      resultList2 `shouldSatisfy` Data.List.all (== 2)

pubSubTest' ::
     forall transport. HasPubSubTransport transport T
  => Spec
pubSubTest' =
  around (withSubscribed @transport (Proxy :: Proxy (IncrementTopic transport))) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Streamly.toList (Streamly.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

directTests ::
     forall transport. HasRpcTransport transport T
  => [Spec]
directTests = [singleDirectTest @transport, multipleDirectTest @transport]

streamingTests ::
     forall transport. HasRpcTransport transport T
  => [Spec]
streamingTests =
  [singleStreamingTest @transport, multipleStreamingTest @transport]

pubSubTests ::
     forall transport. HasPubSubTransport transport T
  => [Spec]
pubSubTests = [pubSubTest' @transport]

webSocketTestConfig :: WebSocket.Config
webSocketTestConfig =
  WebSocket.localConfig
    { WebSocket.servers =
        [ WebSocket.ServerInfo
            { uri = Tagged "127.0.0.1"
            , port = Tagged 3000
            , path = Tagged "/"
            , routes =
                map
                  (namespaced' @T . Tagged . pack)
                  (symbolVals (Proxy :: Proxy (Routes (TestRpcApi WebSocket.T))))
            }
        ]
    }

spec :: Spec
spec = do
  describe "Empty API" $ do
    describe "RPC" emptyRpcTest
    describe "PubSub" emptyPubSubTest
  describe "AMQP Vest.Bridge" $ do
    describe "Direct RPC" $ mapM_ identity (directTests @Amqp.T)
    describe "Streaming RPC" $ mapM_ identity (streamingTests @Amqp.T)
    describe "Pub/Sub" $ mapM_ identity (pubSubTests @Amqp.T)
  describe "WebSocket Vest.Bridge" $ do
    describe "Direct RPC" $ mapM_ identity (directTests @WebSocket.T)
    describe "Streaming RPC" $ mapM_ identity (streamingTests @WebSocket.T)
