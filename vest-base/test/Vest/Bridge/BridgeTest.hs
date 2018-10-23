module Vest.Bridge.BridgeTest
  ( test_bridge
  ) where

import qualified Stream
import Test
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

-- TODO: Add test for HeartbeatLostExceptions.
-- TODO: Add test for server exceptions.
data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  }

-- This instance is not really overlapping but for some reason GHC thinks it is.
instance {-# OVERLAPPING #-} HasNamespace T where
  namespace = "test"

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasPubSubTransport Amqp.T T where
  pubSubTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis

type EchoIntsDirectEndpoint transport
   = Endpoint 'NoAuth T transport "echoIntsDirect" [Int] ('Direct [Int])

type EchoTextsDirectEndpoint transport
   = Endpoint 'NoAuth T transport "echoTextDirect" [Text] ('Direct [Text])

type EchoIntsStreamingEndpoint transport
   = Endpoint 'NoAuth T transport "echoIntsStreaming" [Int] ('Streaming Int)

type EchoTextsStreamingEndpoint transport
   = Endpoint 'NoAuth T transport "echoTextsStreaming" [Text] ('Streaming Text)

type TimeoutEndpoint transport
   = Endpoint_ 0 'Haskell 'NoAuth T transport "timeout" () ('Direct ())

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
          [ ( namespace' @T @"Server"
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
  with localRedisConfig $ \redis ->
    with webSocketConfig $ \webSocket ->
      with Amqp.localConfig (\amqp -> f $ T {amqp, webSocket, redis})

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
     forall transport spec a. (HasRpcTransport transport T, Client T spec)
  => Proxy spec
  -> (ClientBindings spec -> IO a)
  -> IO a
withRpcClient _ f =
  withT $ \t -> do
    serve t (Proxy :: Proxy (TestRpcApi transport)) handlers
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

type IncrementValueTopic transport
   = Topic 'Value T transport "incrementValue" Int

-- type IncrementEventTopic transport
--    = Topic T transport "incrementEvent" ('Event Int)
type TestPubSubApi transport = IncrementValueTopic transport --  :<|> IncrementEventTopic transport

streams :: Streams (TestPubSubApi Amqp.T)
streams = increment -- :<|> increment

withSubscribed ::
     forall transport spec a.
     (HasPubSubTransport transport T, Subscriber T spec)
  => Proxy spec
  -> (SubscriberBindings spec -> IO a)
  -> IO a
withSubscribed _ f =
  withT $ \t -> do
    subscribed <- subscribe t (Proxy :: Proxy spec)
    publish t (Proxy :: Proxy (TestPubSubApi transport)) streams
    f subscribed

emptyRpcTest :: TestTree
emptyRpcTest =
  testCase "RPC" "test/Vest/Bridge/empty-rpc.gold" $ withT $ \t -> do
    serve t (Proxy :: Proxy ()) ()
    () <- return $ makeClient t (Proxy :: Proxy ())
    return ""

emptyPubSubTest :: TestTree
emptyPubSubTest =
  testCase "PubSub" "test/Vest/Bridge/empty-pubsub.gold" $ withT $ \t -> do
    () <- subscribe t (Proxy :: Proxy ())
    publish t (Proxy :: Proxy ()) ()
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
    result <- newTVarIO ""
    call [1, 2, 3] $ \results -> do
      last1 <- Stream.toList results >>- last
      last2 <- Stream.toList results >>- last
      atomically $ writeTVar result (show (last1, last2))
    readTVarIO result

multipleStreamingTest ::
     forall transport. HasRpcTransport transport T
  => TestTree
multipleStreamingTest =
  testCase "Multiple" "test/Vest/Bridge/streaming-rpc-multiple.gold" $
  withRpcClient
    @transport
    (Proxy :: Proxy (EchoIntsStreamingEndpoint transport
                     :<|> EchoTextsStreamingEndpoint transport)) $ \(echoInts :<|> echoTexts) -> do
    result <- newTVarIO ""
    echoInts (replicate 3 4) $ \resultInts1 ->
      echoInts (replicate 3 5) $ \resultInts2 ->
        echoTexts (replicate 3 "a") $ \resultTexts -> do
          lastInt1 <- Stream.toList resultInts1 >>- last
          lastInt2 <- Stream.toList resultInts2 >>- last
          lastText <- Stream.toList resultTexts >>- last
          atomically $ writeTVar result (show (lastInt1, lastInt2, lastText))
    readTVarIO result

-- eventPubSubTest ::
--      forall transport. HasPubSubTransport transport T
--   => TestTree
-- eventPubSubTest =
--   testCase "Event" "test/Vest/Bridge/pubsub-event.gold" $
--   withSubscribed @transport (Proxy :: Proxy (IncrementEventTopic transport)) $ \getEvents -> do
--     getEvents
--     value <- atomically getValue
--     return $ show value
valuePubSubTest ::
     forall transport. HasPubSubTransport transport T
  => TestTree
valuePubSubTest =
  testCase "Value" "test/Vest/Bridge/pubsub-value.gold" $
  withSubscribed @transport (Proxy :: Proxy (IncrementValueTopic transport)) $ \(_, peekValue) -> do
    threadDelay $ sec 0.1
    value <- atomically peekValue
    return $ show value

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
pubSubTests = testGroup "PubSub" [valuePubSubTest @transport]

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
