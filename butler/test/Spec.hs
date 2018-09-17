import Butler ((:<|>)(..), Protocol, ProtocolJSON, Publishing, PublishingJSON)
import qualified Butler
import qualified Control.Concurrent.MVar as MVar
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified System.Random as Random
import Test.Hspec
import VestPrelude

double :: Int -> IO Int
double = return . (2 *)

echo :: Text -> IO Text
echo = return

doubleSum :: [Int] -> IO Int
doubleSum = return . (2 *) . sum

type SimpleAPI
   = Protocol "double" Int Int :<|> Protocol "echo" Text Text :<|> ProtocolJSON "doubleSum" [Int] Int

proxySimpleAPI :: Proxy SimpleAPI
proxySimpleAPI = Proxy

handleSimpleAPI :: Butler.Server SimpleAPI
handleSimpleAPI = double :<|> echo :<|> doubleSum

_testSimpleAPI :: IO Spec
_testSimpleAPI = do
  routeVar <- MVar.newEmptyMVar
  messageVar <- MVar.newEmptyMVar
  resultVar <- MVar.newEmptyMVar
  let router = Butler.makeRouter proxySimpleAPI handleSimpleAPI
  let callDouble :<|> callEcho :<|> callDoubleSum =
        Butler.makeClient
          proxySimpleAPI
          (\route message -> do
             MVar.putMVar routeVar route
             MVar.putMVar messageVar message
             MVar.takeMVar resultVar)
  -- Simple server thread.
  serverThread <-
    async . forever $ do
      route <- MVar.takeMVar routeVar
      message <- MVar.takeMVar messageVar
      server <- router route
      result <- server message
      MVar.putMVar resultVar result
  a <- callDouble 3
  b <- callEcho "test"
  c <- callDoubleSum [1, 2, 3, 4]
  cancel serverThread
  return . describe "simple API" $
    it "should compute results correctly" $ (a, b, c) `shouldBe` (6, "test", 20)

doubleStream :: Int -> IO (Streamly.Serial Int)
doubleStream x = return . Streamly.fromList $ fmap (x *) [1, 2, 3]

echoStream :: Text -> IO (Streamly.Serial Text)
echoStream = return . Streamly.fromList . replicate 3

type StreamAPI = Protocol "double" Int Int :<|> Protocol "echo" Text Text

proxyStreamAPI :: Proxy StreamAPI
proxyStreamAPI = Proxy

handleStreamAPI :: Butler.Server' StreamAPI
handleStreamAPI = doubleStream :<|> echoStream

_testStreamAPI :: IO Spec
_testStreamAPI = do
  routeVar <- MVar.newEmptyMVar
  messageVar <- MVar.newEmptyMVar
  resultsVar <- MVar.newEmptyMVar
  let router = Butler.makeRouter' proxyStreamAPI handleStreamAPI
  let callDouble' :<|> callEcho' =
        Butler.makeClient'
          proxyStreamAPI
          (\route message -> do
             rp <- repeatableStream
             let (_, _, stream) = rp
             MVar.putMVar routeVar route
             MVar.putMVar messageVar message
             MVar.putMVar resultsVar rp
             return stream)
  -- Stream server thread.
  serverThread <-
    async . forever $ do
      route <- MVar.takeMVar routeVar
      message <- MVar.takeMVar messageVar
      (push, close, _) <- MVar.takeMVar resultsVar
      server <- router route
      results <- server message
      Streamly.mapM_ push results
      close
  a <- callDouble' 3
  b <- callEcho' "test"
  as <- Streamly.toList a
  bs <- Streamly.toList b
  cancel serverThread
  return . describe "stream API" $
    it "should compute results correctly" $
    (as, bs) `shouldBe` ([3, 6, 9], ["test", "test", "test"])

type PublishAPI = PublishingJSON "increment" Int :<|> Publishing "repeat" Text

proxyPublishAPI :: Proxy PublishAPI
proxyPublishAPI = Proxy

_testPublishAPI :: IO Spec
_testPublishAPI = do
  (pushInc, closeInc, streamInc) <- repeatableStream
  (pushRep, closeRep, streamRep) <- repeatableStream
  let publishIncrement' :<|> publishRepeat' =
        Butler.makePublish'
          proxyPublishAPI
          (\route message ->
             case route of
               Route "increment" -> pushInc message
               Route "repeat" -> pushRep message
               _ -> return ())
  let subscribeIncrement' :<|> subscribeRepeat' =
        Butler.makeSubscribe'
          proxyPublishAPI
          (\route -> do
             idInt <- Random.randomRIO (1, 10000)
             let id = Id . show $ (idInt :: Int)
             return $
               case route of
                 Route "increment" -> (id, streamInc)
                 Route "repeat" -> (id, streamRep)
                 _ -> (id, Streamly.nil))
  (_, resultsInc) <- subscribeIncrement'
  (_, resultsRep) <- subscribeRepeat'
  async $ do
    publishIncrement' (Streamly.fromList [1, 2, 3, 4])
    closeInc
  async $ do
    publishRepeat' (Streamly.fromList ["test", "test", "test"])
    closeRep
  resultListInc <- Streamly.toList resultsInc
  resultListRep <- Streamly.toList resultsRep
  return . describe "publish API" $
    it "should compute results correctly" $
    (resultListInc, resultListRep) `shouldBe`
    ([1, 2, 3, 4], ["test", "test", "test"])

main :: IO ()
main = do
  testSimpleAPI <- _testSimpleAPI
  testStreamAPI <- _testStreamAPI
  testPublishAPI <- _testPublishAPI
  hspec $ do
    testSimpleAPI
    testStreamAPI
    testPublishAPI
