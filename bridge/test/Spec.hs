import Bridge
import qualified Bridge.Rpc.Auth.Token as Token
import qualified Bridge.Transports.Amqp as Amqp

import Bridge.Rpc.Prelude ()
import qualified Bridge.Transports.WebSocket as WebSocket
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

data DummyService = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  }

instance Service T where
  type ServiceArgs T = DummyService
  defaultArgs = Args {}
  run _ f =
    with webSocketTestConfig $ \webSocket ->
      with Amqp.localConfig (\amqp -> f $ T {amqp, webSocket})

type EchoIntsDirectEndpoint
   = Endpoint T 'NoAuth "echoIntsDirect" [Int] ('Direct [Int])

type EchoTextsDirectEndpoint
   = Endpoint T 'NoAuth "echoTextDirect" [Text] ('Direct [Text])

type EchoIntsStreamingEndpoint
   = Endpoint T ('Auth Token.T) "echoIntsStreaming" [Int] ('Streaming Int)

type EchoTextsStreamingEndpoint
   = Endpoint T ('Auth Token.T) "echoTextsStreaming" [Text] ('Streaming Text)

type RpcApi
   = EchoIntsDirectEndpoint
     :<|> EchoTextsDirectEndpoint
     :<|> EchoIntsStreamingEndpoint
     :<|> EchoTextsStreamingEndpoint

echoDirect :: T -> a -> IO a
echoDirect _ x = threadDelay (sec 0.01) >> return x

echoStreaming ::
     forall a auth. T -> AuthClaims auth -> [a] -> IO (Streamly.Serial a)
echoStreaming _ _ xs =
  return $ Streamly.fromList xs & Streamly.mapM (<$ threadDelay (sec 0.01))

handlers :: Handlers RpcApi
handlers =
  echoDirect :<|> echoDirect :<|> echoStreaming :<|> echoStreaming @Text

withRpcClient ::
     forall service spec transport.
     (Server service RpcApi transport, Client spec transport)
  => Proxy (spec, transport)
  -> (service -> transport)
  -> (ClientBindings spec -> IO ())
  -> IO ()
withRpcClient _ getTransport action =
  run
    @service
    (defaultArgs @service)
    (\service -> do
       serve
         handlers
         service
         (Proxy :: Proxy (RpcApi, transport))
         (getTransport service)
       threadDelay (sec 0.1) -- Wait for servers to initialize and avoid races.
       action $
         makeClient (Proxy :: Proxy (spec, transport)) (getTransport service))

increment :: Streamly.Serial Int
increment =
  let f s = do
        threadDelay (sec 0.01)
        return $
          if s < 5
            then Just (s, s + 1 :: Int)
            else Nothing
   in Streamly.unfoldrM f 0

type IncrementTopic = Topic T 'Haskell "increment" Int

type PubSubApi = IncrementTopic

streams :: Streams PubSubApi
streams = increment

withSubscribed ::
     forall service spec transport.
     (Publisher service PubSubApi transport, Subscriber spec transport)
  => Proxy (spec, transport)
  -> (service -> transport)
  -> (SubscriberBindings spec -> IO ())
  -> IO ()
withSubscribed _ getTransport action =
  run
    @service
    (defaultArgs @service)
    (\service -> do
       subscribed <-
         subscribe (Proxy :: Proxy (spec, transport)) (getTransport service)
       publish
         @service
         streams
         (Proxy :: Proxy (PubSubApi, transport))
         (getTransport service)
       action subscribed)

tokenAuthJSON :: Headers
tokenAuthJSON = Headers {format = JSON, token = Just $ Tagged ""}

singleDirectTest ::
     Server service RpcApi transport => (service -> transport) -> Spec
singleDirectTest getTransport =
  around
    (withRpcClient (Proxy :: Proxy (EchoIntsDirectEndpoint, a)) getTransport) $
  context "with a single direct RPC" $ do
    it "makes a single call" $ \call -> do
      result <- call (sec 1) defaultHeaders [1, 2, 3]
      result `shouldBe` [1, 2, 3]
    it "times out for a single call" $ \call ->
      call (sec 0) defaultHeaders [1, 2, 3] `shouldThrow`
      (== TimeoutException (sec 0))

singleStreamingTest ::
     Server service RpcApi transport => (service -> transport) -> Spec
singleStreamingTest getTransport =
  around
    (withRpcClient (Proxy :: Proxy (EchoIntsStreamingEndpoint, a)) getTransport) $
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

multipleDirectTest ::
     Server service RpcApi transport => (service -> transport) -> Spec
multipleDirectTest getTransport =
  around
    (withRpcClient
       (Proxy :: Proxy ( EchoIntsDirectEndpoint
                         :<|> EchoTextsDirectEndpoint
                       , t))
       getTransport) $
  context "when running multiple direct RPCs" $
  it "makes one call to each" $ \(echoInts :<|> echoTexts) -> do
    resultInts <- echoInts (sec 1) defaultHeaders [1, 2, 3]
    resultTexts <- echoTexts (sec 1) defaultHeaders ["a", "b", "c"]
    resultInts `shouldBe` [1, 2, 3]
    resultTexts `shouldBe` ["a", "b", "c"]

multipleStreamingTest ::
     Server service RpcApi transport => (service -> transport) -> Spec
multipleStreamingTest getTransport =
  around
    (withRpcClient
       (Proxy :: Proxy ( EchoIntsStreamingEndpoint
                         :<|> EchoTextsStreamingEndpoint
                       , transport))
       getTransport) $
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

pubSubTest' ::
     Publisher service PubSubApi transport => (service -> transport) -> Spec
pubSubTest' getTransport =
  around (withSubscribed (Proxy :: Proxy (IncrementTopic, a)) getTransport) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Streamly.toList (Streamly.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

directTests ::
     Server service RpcApi transport => [(service -> transport) -> Spec]
directTests = [singleDirectTest, multipleDirectTest]

streamingTests ::
     Server service RpcApi transport => [(service -> transport) -> Spec]
streamingTests = [singleStreamingTest, multipleStreamingTest]

pubSubTests ::
     Publisher service PubSubApi transport => [(service -> transport) -> Spec]
pubSubTests = [pubSubTest']

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
                  (namespace @T . Tagged . pack)
                  (symbolVals (Proxy :: Proxy (Routes RpcApi)))
            }
        ]
    }

main :: IO ()
main =
  hspec $ do
    describe "AMQP bridge" $ do
      describe "Direct RPC" $ mapM_ ($ amqp) directTests
      describe "Streaming RPC" $ mapM_ ($ amqp) streamingTests
      describe "Pub/Sub" $ mapM_ ($ amqp) pubSubTests
    describe "WebSocket bridge" $ do
      describe "Direct RPC" $ mapM_ ($ webSocket) directTests
      describe "Streaming RPC" $ mapM_ ($ webSocket) streamingTests
