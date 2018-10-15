module Vest.Bridge.BridgeSpec
  ( spec
  ) where

import qualified Stream
import Test.Hspec
import qualified Transports.Amqp as Amqp
import qualified Transports.WebSocket as WebSocket
import Vest.Bridge
import Vest.Bridge.Rpc.Prelude ()
import Vest.Prelude

-- TODO: refactor to tasty-golden tests
-- TODO: add test for HeartbeatLostExceptions
-- For some reason this gets an overlapping HasNamespace T instance if it imports Vest.Service at all
data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  }

instance HasNamespace T where
  namespace = Tagged "test"

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasPubSubTransport Amqp.T T where
  pubSubTransport = amqp

type EchoIntsDirectEndpoint transport
   = Endpoint 'NoAuth T transport "echoIntsDirect" [Int] ('Direct [Int])

type EchoTextsDirectEndpoint transport
   = Endpoint 'NoAuth T transport "echoTextDirect" [Text] ('Direct [Text])

type EchoIntsStreamingEndpoint transport
   = Endpoint 'NoAuth T transport "echoIntsStreaming" [Int] ('Streaming Int)

type EchoTextsStreamingEndpoint transport
   = Endpoint 'NoAuth T transport "echoTextsStreaming" [Text] ('Streaming Text)

type TimeoutEndpoint transport
   = Endpoint_ 0 "Haskell" 'NoAuth T transport "timeout" () ('Direct ())

type TestRpcApi transport
   = EchoIntsDirectEndpoint transport
     :<|> EchoTextsDirectEndpoint transport
     :<|> EchoIntsStreamingEndpoint transport
     :<|> EchoTextsStreamingEndpoint transport
     :<|> TimeoutEndpoint transport

withT :: (T -> IO a) -> IO a
withT f =
  with webSocketTestConfig $ \webSocket ->
    with Amqp.localConfig (\amqp -> f $ T {amqp, webSocket})

echoDirect :: T -> a -> IO a
echoDirect _ = return

echoStreaming :: T -> [a] -> IO (Stream a)
echoStreaming _ xs =
  return $ Stream.fromList xs & Stream.mapM (<$ threadDelay (sec 0.01))

handlers :: Handlers (TestRpcApi Amqp.T)
handlers =
  echoDirect :<|> echoDirect :<|> echoStreaming :<|> echoStreaming :<|>
  (\_ () -> threadDelay (sec 0.01))

withRpcClient ::
     forall transport spec. (Server T (TestRpcApi transport), Client T spec)
  => Proxy spec
  -> (ClientBindings spec -> IO ())
  -> IO ()
withRpcClient _ f =
  withT $ \t -> do
    serve handlers t (Proxy :: Proxy (TestRpcApi transport))
    f $ makeClient t (Proxy :: Proxy spec)

increment :: Stream Int
increment =
  let f s = do
        threadDelay (sec 0.01)
        return $
          if s < 5
            then Just (s, s + 1 :: Int)
            else Nothing
   in Stream.unfoldrM f 0

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
  context "with a single direct RPC" $
  it "makes a single call" $ \call -> do
    result <- call [1, 2, 3]
    result `shouldBe` [1, 2, 3]

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
      done <- newEmptyMVar
      call [1, 2, 3] $ \results -> do
        (Stream.toList results >>- elem 3) >>= (`unless` (throwString "failed"))
        putMVar done ()
      () <- readMVar done
      True `shouldBe` True
    it "sees the last item for every fanout" $ \call -> do
      done <- newEmptyMVar
      call [1, 2, 3] $ \results -> do
        (Stream.toList results >>- elem 3) >>= (`unless` (throwString "failed"))
        (Stream.toList results >>- elem 3) >>= (`unless` (throwString "failed"))
        putMVar done ()
      () <- readMVar done
      True `shouldBe` True

multipleDirectTest ::
     forall transport. HasRpcTransport transport T
  => Spec
multipleDirectTest =
  around
    (withRpcClient
       @transport
       (Proxy :: Proxy (EchoIntsDirectEndpoint transport
                        :<|> EchoTextsDirectEndpoint transport
                        :<|> TimeoutEndpoint transport))) $
  context "when running multiple direct RPCs" $ do
    it "makes one call to each" $ \(echoInts :<|> echoTexts :<|> _) -> do
      resultInts <- echoInts [1, 2, 3]
      resultTexts <- echoTexts ["a", "b", "c"]
      resultInts `shouldBe` [1, 2, 3]
      resultTexts `shouldBe` ["a", "b", "c"]
    it "times out for a single call" $ \(_ :<|> _ :<|> call) ->
      call () `shouldThrow` (== TimeoutException (sec 0))

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
      done <- newEmptyMVar
      echoInts [1, 2, 3] $ \ints ->
        echoTexts ["a", "b", "c"] $ \texts -> do
          (Stream.toList ints >>- elem 3) >>= (`unless` (throwString "failed"))
          (Stream.toList texts >>- elem "c") >>=
            (`unless` (throwString "failed"))
          putMVar done ()
      () <- readMVar done
      True `shouldBe` True
    it "handles concurrent calls" $ \(echoInts :<|> _) -> do
      done <- newEmptyMVar
      echoInts (replicate 3 1) $ \r1 ->
        echoInts (replicate 3 2) $ \r2 -> do
          (Stream.toList r1 >>- all (== 1)) >>=
            (`unless` (throwString "failed"))
          (Stream.toList r2 >>- all (== 2)) >>=
            (`unless` (throwString "failed"))
          putMVar done ()
      () <- readMVar done
      True `shouldBe` True

pubSubTest' ::
     forall transport. HasPubSubTransport transport T
  => Spec
pubSubTest' =
  around (withSubscribed @transport (Proxy :: Proxy (IncrementTopic transport))) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Stream.toList (Stream.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

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
                  (namespaced' @T)
                  (symbolTexts'
                     (Proxy :: Proxy (Routes (TestRpcApi WebSocket.T))))
            }
        ]
    }

spec :: Spec
spec = do
  describe "Empty API" $ do
    describe "RPC" emptyRpcTest
    describe "PubSub" emptyPubSubTest
  describe "Bridge Framework and AMQP Transport" $ do
    describe "Direct RPC" $ mapM_ identity (directTests @Amqp.T)
    describe "Streaming RPC" $ mapM_ identity (streamingTests @Amqp.T)
    describe "Pub/Sub" $ mapM_ identity (pubSubTests @Amqp.T)
  describe "WebSocket Transport" $ mapM_ identity (streamingTests @WebSocket.T)
