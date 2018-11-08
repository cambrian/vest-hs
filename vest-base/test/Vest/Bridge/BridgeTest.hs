module Vest.Bridge.BridgeTest
  ( test_bridge
  ) where

import Test
import qualified Transport.Amqp as Amqp
import qualified Transport.WebSocket as WebSocket
import Vest

-- TODO: Add test for HeartbeatLostExceptions.
-- TODO: Add test for server exceptions.
-- TODO: Add test for Event bridge
data T = T
  { amqp :: Amqp.T
  , webSocket :: WebSocket.T
  , redis :: RedisConnection
  } deriving (HasNamespace)

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasRpcTransport WebSocket.T T where
  rpcTransport = webSocket

instance HasValueTransport Amqp.T T where
  valueTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis

instance HasLogger T where
  logger _ = stderrLogger Debug

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
  with defaultRedisConfig $ \redis ->
    with webSocketConfig $ \webSocket ->
      with Amqp.localConfig (\amqp -> f $ T {amqp, webSocket, redis})

echoDirect :: a -> IO a
echoDirect = return

echoStreaming :: (Eq a) => [a] -> IO (Stream ValueBuffer a)
echoStreaming xs = streamFromList xs >>= mapMStream (<$ threadDelay (sec 0.01))

handlers :: Handlers (TestRpcApi Amqp.T)
handlers =
  echoDirect :<|> echoDirect :<|> echoStreaming :<|> echoStreaming :<|>
  const (threadDelay (sec 0.01))

withRpcClient ::
     forall transport spec a. (HasRpcTransport transport T, Client T spec)
  => Proxy spec
  -> (ClientBindings spec -> IO a)
  -> IO a
withRpcClient _ f =
  withT $ \t -> do
    serve t (Proxy :: Proxy (TestRpcApi transport)) handlers
    f $ makeClient t (Proxy :: Proxy spec)

makeIncrementValue :: IO (Stream ValueBuffer Int)
makeIncrementValue = do
  (writer, stream) <- newStream
  let loop stop n =
        when (n < stop) $ do
          writeStream writer n
          threadDelay (sec 0.01)
          loop stop (n + 1)
  async $ loop 5 0
  return stream

type IncrementValueTopic transport = ValueTopic T transport "incrementValue" Int

-- type IncrementEventValue transport
--    = Value T transport "incrementEvent" ('Event Int)
type TestValueApi transport = IncrementValueTopic transport --  :<|> IncrementEventValue transport

makeValues :: IO (Values (TestValueApi Amqp.T))
makeValues = makeIncrementValue

withSubscribed ::
     forall transport spec a. (HasValueTransport transport T, Subscriber T spec)
  => Proxy spec
  -> (SubscriberBindings spec -> IO a)
  -> IO a
withSubscribed _ f =
  withT $ \t -> do
    subscribed <- subscribe t (Proxy :: Proxy spec)
    values <- makeValues
    publish t (Proxy :: Proxy (TestValueApi transport)) values
    f subscribed

emptyRpcTest :: TestTree
emptyRpcTest =
  testCase "RPC" "test/Vest/Bridge/empty-rpc.gold" $ withT $ \t -> do
    serve t (Proxy :: Proxy ()) ()
    () <- return $ makeClient t (Proxy :: Proxy ())
    return ""

emptyValueTest :: TestTree
emptyValueTest =
  testCase "Value" "test/Vest/Bridge/empty-pubsub.gold" $ withT $ \t -> do
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
      v <- readFinalValue results
      atomically $ writeTVar result $ show v
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
          int1 <- readFinalValue resultInts1
          int2 <- readFinalValue resultInts2
          text <- readFinalValue resultTexts
          atomically $ writeTVar result (show (int1, int2, text))
    readTVarIO result

-- eventValueTest ::
--      forall transport. HasValueTransport transport T
--   => TestTree
-- eventValueTest =
--   testCase "Event" "test/Vest/Bridge/pubsub-event.gold" $
--   withSubscribed @transport (Proxy :: Proxy (IncrementEventValue transport)) $ \getEvents -> do
--     getEvents
--     value <- atomically getValue
--     return $ show value
valueTest ::
     forall transport. HasValueTransport transport T
  => TestTree
valueTest =
  testCase "Value" "test/Vest/Bridge/pubsub-value.gold" $
  withSubscribed @transport (Proxy :: Proxy (IncrementValueTopic transport)) $ \stream -> do
    threadDelay $ sec 0.1
    value <- readLatestValue stream
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

valueTests ::
     forall transport. HasValueTransport transport T
  => TestTree
valueTests = testGroup "Value" [valueTest @transport]

test_bridge :: TestTree
test_bridge =
  testGroup
    "Bridge"
    [ testGroup "Empty API" [emptyRpcTest, emptyValueTest]
    , testGroup
        "Full Bridge API over AMQP Transport"
        [directTests @Amqp.T, streamingTests @Amqp.T, valueTests @Amqp.T]
    , testGroup
        "WebSocket Transport"
        [directTests @WebSocket.T, streamingTests @WebSocket.T]
    ]
