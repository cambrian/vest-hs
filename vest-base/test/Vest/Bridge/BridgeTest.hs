module Vest.Bridge.BridgeTest
  ( test_bridge
  ) where

import qualified Stream
import Test
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

-- TODO: add test for HeartbeatLostExceptions
data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  }

-- This instance is not really overlapping but for some reason GHC thinks it is.
instance {-# OVERLAPPING #-} HasNamespace T where
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

makeWebSocketConfig :: IO WebSocket.Config
makeWebSocketConfig = do
  portNum <- nextCount >>- (+ 13000) . fromIntegral
  return $
    WebSocket.localConfig
      { WebSocket.servers =
          [ ( namespace @T
            , WebSocket.ServerInfo
                { uri = Tagged "127.0.0.1"
                , port = Tagged portNum
                , path = Tagged "/"
                })
          ]
      , WebSocket.servePort = Tagged portNum
      }

withT :: (T -> IO a) -> IO a
withT f = do
  webSocketConfig <- makeWebSocketConfig
  with webSocketConfig $ \webSocket ->
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
     forall transport spec a. (Server T (TestRpcApi transport), Client T spec)
  => Proxy spec
  -> (ClientBindings spec -> IO a)
  -> IO a
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
     forall transport spec a.
     (Publisher T (TestPubSubApi transport), Subscriber T spec)
  => Proxy spec
  -> (SubscriberBindings spec -> IO a)
  -> IO a
withSubscribed _ f =
  withT $ \t -> do
    subscribed <- subscribe t (Proxy :: Proxy spec)
    publish streams t (Proxy :: Proxy (TestPubSubApi transport))
    f subscribed

emptyRpcTest :: TestTree
emptyRpcTest =
  testCase "RPC" "test/Vest/Bridge/empty-rpc.gold" $ withT $ \t -> do
    serve () t (Proxy :: Proxy ())
    () <- return $ makeClient t (Proxy :: Proxy ())
    return ""

emptyPubSubTest :: TestTree
emptyPubSubTest =
  testCase "PubSub" "test/Vest/Bridge/empty-pubsub.gold" $ withT $ \t -> do
    () <- subscribe t (Proxy :: Proxy ())
    publish () t (Proxy :: Proxy ())
    return ""

singleDirectTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
singleDirectTest =
  testCase "Single" "test/Vest/Bridge/direct-rpc-single.gold" $
  withRpcClient @transport (Proxy :: Proxy (EchoIntsDirectEndpoint transport)) $ \call -> do
    result <- call [1, 2, 3]
    return $ show result

multipleDirectTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
multipleDirectTest =
  testCase "Multiple" "test/Vest/Bridge/direct-rpc-multiple.gold" $
  withRpcClient
    @transport
    (Proxy :: Proxy (EchoIntsDirectEndpoint transport
                     :<|> EchoTextsDirectEndpoint transport)) $ \(echoInts :<|> echoTexts) -> do
    resultInts <- echoInts [1, 2, 3]
    resultTexts <- echoTexts ["a", "b", "c"]
    return $ show (resultInts, resultTexts)

timeoutTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
timeoutTest =
  expectFail $ testCase "Timeout" "test/Vest/Bridge/timeout.gold" $
  withRpcClient @transport (Proxy :: Proxy (TimeoutEndpoint transport)) $ \call -> do
    call ()
    return ""

singleStreamingTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
singleStreamingTest =
  testCase "Single" "test/Vest/Bridge/streaming-rpc-single.gold" $
  withRpcClient
    @transport
    (Proxy :: Proxy (EchoIntsStreamingEndpoint transport)) $ \call -> do
    result <- newEmptyMVar
    call [1, 2, 3] $ \results -> do
      last1 <- Stream.toList results >>- last
      last2 <- Stream.toList results >>- last
      putMVar result (show (last1, last2))
    takeMVar result

multipleStreamingTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
multipleStreamingTest =
  testCase "Multiple" "test/Vest/Bridge/streaming-rpc-multiple.gold" $
  withRpcClient
    @transport
    (Proxy :: Proxy (EchoIntsStreamingEndpoint transport
                     :<|> EchoTextsStreamingEndpoint transport)) $ \(echoInts :<|> echoTexts) -> do
    result <- newEmptyMVar
    echoInts (replicate 3 4) $ \resultInts1 ->
      echoInts (replicate 3 5) $ \resultInts2 ->
        echoTexts (replicate 3 "a") $ \resultTexts -> do
          lastInt1 <- Stream.toList resultInts1 >>- last
          lastInt2 <- Stream.toList resultInts2 >>- last
          lastText <- Stream.toList resultTexts >>- last
          putMVar result (show (lastInt1, lastInt2, lastText))
    takeMVar result

simplePubSubTest ::
     forall transport. HasPubSubTransport transport T
  => TestTree
simplePubSubTest =
  testCase "Simple" "test/Vest/Bridge/pubsub-simple.gold" $
  withSubscribed @transport (Proxy :: Proxy (IncrementTopic transport)) $ \(_id, results) -> do
    results <- Stream.toList $ Stream.take 5 results
    return $ show results

directTests ::
     forall transport. HasRpcTransport transport T
  => TestTree
directTests =
  testGroup
    "Direct RPC"
    [ singleDirectTest @transport
    , multipleDirectTest @transport
    , timeoutTest @transport
    ]

streamingTests ::
     forall transport. HasRpcTransport transport T
  => TestTree
streamingTests =
  testGroup
    "Streaming RPC"
    [singleStreamingTest @transport, multipleStreamingTest @transport]

pubSubTests ::
     forall transport. HasPubSubTransport transport T
  => TestTree
pubSubTests = testGroup "PubSub" [simplePubSubTest @transport]

test_bridge :: TestTree
test_bridge =
  testGroup
    "Bridge"
    [ testGroup "Empty API" [emptyRpcTest, emptyPubSubTest]
    , testGroup
        "Full Bridge API over AMQP Transport"
        [directTests @Amqp.T, streamingTests @Amqp.T, pubSubTests @Amqp.T]
    , testGroup
        "WebSocket Transport"
        [directTests @WebSocket.T, streamingTests @WebSocket.T]
    ]
