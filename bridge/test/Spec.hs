import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified Bridge.Transports.Dummy as Dummy
import qualified Bridge.Transports.WebSocket as WebSocket
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

type EchoEndpoint = DirectEndpoint "echo" Text Text

type EchoIntsEndpoint = DirectEndpointJSON "echoInts" [Int] [Int]

type EchoInts'Endpoint = StreamingEndpoint "echoInts'" [Int] Int

type EchoCharsEndpoint = DirectEndpoint "echoChars" [Char] [Char]

type EchoChars'Endpoint = StreamingEndpoint "echoChars'" [Char] Char

type EchoTimeoutEndpoint = DirectEndpoint "echoTimeout" [Int] [Int]

type EchoTimeout'Endpoint = StreamingEndpoint "echoTimeout'" [Int] Int

type EchoDelay'Endpoint = StreamingEndpoint "echoDelay'" Int Int

type RpcApi
   = EchoEndpoint
     :<|> EchoIntsEndpoint
     :<|> EchoInts'Endpoint
     :<|> EchoCharsEndpoint
     :<|> EchoChars'Endpoint
     :<|> EchoTimeoutEndpoint
     :<|> EchoTimeout'Endpoint
     :<|> EchoDelay'Endpoint

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

handlers :: Handlers RpcApi
handlers =
  echo :<|> echoInts :<|> echoInts' :<|> echoChars :<|> echoChars' :<|>
  echoTimeout :<|>
  echoTimeout' :<|>
  echoDelay'

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
       serve (Proxy :: Proxy (RpcApi, transport)) transport handlers
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

type IncrementTopic = Topic "increment" Int

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
       publish (Proxy :: Proxy (PubSubApi, transport)) transport streams
       action subscribed)

echoTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
echoTest config =
  around (withRpcClient config (Proxy :: Proxy (EchoEndpoint, a))) $
  context "when running simple echo RPC" $
  it "returns the original output" $ \callEcho -> do
    result <- callEcho (sec 1) "test"
    result `shouldBe` "test"

echoTest' :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
echoTest' config =
  around (withRpcClient config (Proxy :: Proxy (EchoInts'Endpoint, a))) $
  context "when running simple echoInts' RPC" $
  it "returns the original output" $ \callEchoInts' -> do
    results <- callEchoInts' (sec 1) [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]

multipleFnTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
multipleFnTest config =
  around
    (withRpcClient
       config
       (Proxy :: Proxy ( EchoIntsEndpoint
                         :<|> EchoCharsEndpoint
                       , a))) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \(callEchoInts :<|> callEchoChars) -> do
    resultInts <- callEchoInts (sec 1) [1, 2, 3]
    resultChars <- callEchoChars (sec 1) ['a', 'b', 'c']
    resultInts `shouldBe` [1, 2, 3]
    resultChars `shouldBe` ['a', 'b', 'c']

multipleFnTest' :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
multipleFnTest' config =
  around
    (withRpcClient
       config
       (Proxy :: Proxy ( EchoInts'Endpoint
                         :<|> EchoChars'Endpoint
                       , a))) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \(callEchoInts' :<|> callEchoChars') -> do
    resultsInt <- callEchoInts' (sec 1) [1, 2, 3]
    resultsChar <- callEchoChars' (sec 1) ['a', 'b', 'c']
    Streamly.toList resultsInt `shouldReturn` [1, 2, 3]
    Streamly.toList resultsChar `shouldReturn` ['a', 'b', 'c']

multipleConsumeTest' :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
multipleConsumeTest' config =
  around (withRpcClient config (Proxy :: Proxy (EchoInts'Endpoint, a))) $
  context "when running echo with multiple stream consumers" $
  it "returns the original output for both" $ \callEchoInts' -> do
    results <- callEchoInts' (sec 1) [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]

concurrentTest' :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
concurrentTest' config =
  around (withRpcClient config (Proxy :: Proxy (EchoDelay'Endpoint, a))) $
  context "when running RPCs with concurrent callers" $
  it "returns the correct output for both" $ \callEchoDelay' -> do
    results1 <- callEchoDelay' (sec 1) 1
    results2 <- callEchoDelay' (sec 1) 2
    resultList1 <- Streamly.toList results1
    resultList2 <- Streamly.toList results2
    resultList1 `shouldSatisfy` Data.List.all (== 1)
    resultList2 `shouldSatisfy` Data.List.all (== 2)

timeoutTest :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
timeoutTest config =
  around (withRpcClient config (Proxy :: Proxy (EchoTimeoutEndpoint, a))) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \callEchoTimeout ->
    callEchoTimeout (sec 0.01) [1, 2, 3] `shouldThrow`
    (== TimeoutException (sec 0.01))

timeoutTest' :: (Resource a, RpcTransport a) => ResourceConfig a -> Spec
timeoutTest' config =
  around (withRpcClient config (Proxy :: Proxy (EchoTimeout'Endpoint, a))) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \callEchoTimeout' ->
    (do results <- callEchoTimeout' (sec 0.01) [1, 2, 3]
        resultsList <- Streamly.toList results
        print resultsList) `shouldThrow`
    (== TimeoutException (sec 0.01))

pubSubTest' :: (Resource a, PubSubTransport a) => ResourceConfig a -> Spec
pubSubTest' config =
  around (withSubscribed config (Proxy :: Proxy (IncrementTopic, a))) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Streamly.toList (Streamly.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

directTests :: (Resource a, RpcTransport a) => [ResourceConfig a -> Spec]
directTests = [echoTest, multipleFnTest, timeoutTest]

streamingTests :: (Resource a, RpcTransport a) => [ResourceConfig a -> Spec]
streamingTests =
  [ echoTest'
  , multipleFnTest'
  , multipleConsumeTest'
  , concurrentTest'
  , timeoutTest'
  ]

pubSubTests :: (Resource a, PubSubTransport a) => [ResourceConfig a -> Spec]
pubSubTests = [pubSubTest']

main :: IO ()
main = do
  let amqpMake = ($ Amqp.localConfig)
      dummyMake = ($ Dummy.localConfig)
      wsMake = ($ WebSocket.localConfig)
  hspec $ do
    describe "AMQP bridge" $ do
      describe "direct RPC" $ mapM_ amqpMake directTests
      describe "streaming RPC" $ mapM_ amqpMake streamingTests
      describe "publish/subscribe" $ mapM_ amqpMake pubSubTests
    describe "Dummy bridge" $ do
      describe "direct RPC" $ mapM_ dummyMake directTests
      describe "streaming RPC" $ mapM_ dummyMake streamingTests
      describe "publish/subscribe" $ mapM_ dummyMake pubSubTests
    describe "WebSocket bridge" $ do
      describe "direct RPC" $ mapM_ wsMake directTests
      describe "streaming RPC" $ mapM_ wsMake streamingTests
