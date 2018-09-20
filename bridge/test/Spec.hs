import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified Data.List
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Test.Hspec
import VestPrelude

type EchoEndpoint = DirectEndpoint "echo" Text Text

type EchoIntsEndpoint = DirectEndpoint "echoInts" [Int] [Int]

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
    (\(transport :: transport) -> do
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
    (\(transport :: transport) -> do
       subscribed <- subscribe (Proxy :: Proxy (spec, transport)) transport
       publish (Proxy :: Proxy (PubSubApi, transport)) transport streams
       action subscribed)

echoTest :: Spec
echoTest =
  around
    (withRpcClient Amqp.localConfig (Proxy :: Proxy (EchoEndpoint, Amqp.T))) $
  context "when running simple echo RPC" $
  it "returns the original output" $ \callEcho -> do
    result <- callEcho (sec 1) "test"
    result `shouldBe` "test"

echoTest' :: Spec
echoTest' =
  around
    (withRpcClient Amqp.localConfig (Proxy :: Proxy (EchoInts'Endpoint, Amqp.T))) $
  context "when running simple echoInts' RPC" $
  it "returns the original output" $ \callEchoInts' -> do
    results <- callEchoInts' (sec 1) [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]

multipleFnTest :: Spec
multipleFnTest =
  around
    (withRpcClient
       Amqp.localConfig
       (Proxy :: Proxy ( EchoIntsEndpoint
                         :<|> EchoCharsEndpoint
                       , Amqp.T))) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \(callEchoInts :<|> callEchoChars) -> do
    resultInts <- callEchoInts (sec 1) [1, 2, 3]
    resultChars <- callEchoChars (sec 1) ['a', 'b', 'c']
    resultInts `shouldBe` [1, 2, 3]
    resultChars `shouldBe` ['a', 'b', 'c']

multipleFnTest' :: Spec
multipleFnTest' =
  around
    (withRpcClient
       Amqp.localConfig
       (Proxy :: Proxy ( EchoInts'Endpoint
                         :<|> EchoChars'Endpoint
                       , Amqp.T))) $
  context "when running multiple echo RPCs" $
  it "returns the original output for both" $ \(callEchoInts' :<|> callEchoChars') -> do
    resultsInt <- callEchoInts' (sec 1) [1, 2, 3]
    resultsChar <- callEchoChars' (sec 1) ['a', 'b', 'c']
    Streamly.toList resultsInt `shouldReturn` [1, 2, 3]
    Streamly.toList resultsChar `shouldReturn` ['a', 'b', 'c']

multipleConsumeTest' :: Spec
multipleConsumeTest' =
  around
    (withRpcClient Amqp.localConfig (Proxy :: Proxy (EchoInts'Endpoint, Amqp.T))) $
  context "when running echo with multiple stream consumers" $
  it "returns the original output for both" $ \callEchoInts' -> do
    results <- callEchoInts' (sec 1) [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]
    Streamly.toList results `shouldReturn` [1, 2, 3]

concurrentTest' :: Spec
concurrentTest' =
  around
    (withRpcClient
       Amqp.localConfig
       (Proxy :: Proxy (EchoDelay'Endpoint, Amqp.T))) $
  context "when running RPCs with concurrent callers" $
  it "returns the correct output for both" $ \callEchoDelay' -> do
    results1 <- callEchoDelay' (sec 1) 1
    results2 <- callEchoDelay' (sec 1) 2
    resultList1 <- Streamly.toList results1
    resultList2 <- Streamly.toList results2
    resultList1 `shouldSatisfy` Data.List.all (== 1)
    resultList2 `shouldSatisfy` Data.List.all (== 2)

timeoutTest :: Spec
timeoutTest =
  around
    (withRpcClient
       Amqp.localConfig
       (Proxy :: Proxy (EchoTimeoutEndpoint, Amqp.T))) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \callEchoTimeout ->
    callEchoTimeout (sec 0.01) [1, 2, 3] `shouldThrow`
    (== TimeoutException (sec 0.01))

timeoutTest' :: Spec
timeoutTest' =
  around
    (withRpcClient
       Amqp.localConfig
       (Proxy :: Proxy (EchoTimeout'Endpoint, Amqp.T))) $
  context "when running RPCs that take too long" $
  it "forces callers to time out" $ \callEchoTimeout' ->
    (do results <- callEchoTimeout' (sec 0.01) [1, 2, 3]
        resultsList <- Streamly.toList results
        print resultsList) `shouldThrow`
    (== TimeoutException (sec 0.01))

pubSubTest' :: Spec
pubSubTest' =
  around
    (withSubscribed Amqp.localConfig (Proxy :: Proxy (IncrementTopic, Amqp.T))) $
  context "when publishing an incrementing stream" $
  it "functions correctly on the subscribing end" $ \(_id, results) ->
    Streamly.toList (Streamly.take 5 results) `shouldReturn` [0, 1, 2, 3, 4]

main :: IO ()
main =
  hspec $ do
    describe "direct RPC bridge" $ echoTest >> multipleFnTest >> timeoutTest
    describe "streaming RPC bridge" $
      echoTest' >> multipleFnTest' >> multipleConsumeTest' >> concurrentTest' >>
      timeoutTest'
    describe "publish/subscribe bridge" pubSubTest'
