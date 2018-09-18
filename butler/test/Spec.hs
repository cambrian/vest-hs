import Butler ((:<|>)(..), Protocol, ProtocolJSON, Publishing, PublishingJSON)
import qualified Butler
import qualified Control.Concurrent.MVar as MVar
import qualified Data.HashTable.IO as HashTable
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified System.Random as Random
import Test.Hspec
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

defaultTimeout :: Time Second
defaultTimeout = sec 10

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
  requestTable <- HashTable.new -- Route (MVar Text)
  resultTable <- HashTable.new -- Route (MVar Text)
  serverThreads <-
    Butler.makeServer
      proxySimpleAPI
      handleSimpleAPI
      (\route handler -> do
         requestVar <- MVar.newEmptyMVar
         resultVar <- MVar.newEmptyMVar
         HashTable.insert
           (requestTable :: HashTable Route (MVar.MVar Text))
           route
           requestVar
         HashTable.insert
           (resultTable :: HashTable Route (MVar.MVar Text))
           route
           resultVar
         -- Simple server thread per route.
         serverThread <-
           async . forever $ do
             request <- MVar.takeMVar requestVar
             result <- handler request
             MVar.putMVar resultVar result
         return serverThread)
  let callDouble :<|> callEcho :<|> callDoubleSum =
        Butler.makeClient
          proxySimpleAPI
          (\_ _ route request -> do
             requestVarMaybe <- HashTable.lookup requestTable route
             resultVarMaybe <- HashTable.lookup resultTable route
             case (requestVarMaybe, resultVarMaybe) of
               (Just requestVar, Just resultVar) -> do
                 MVar.putMVar requestVar request
                 MVar.takeMVar resultVar
               _ -> panic "this is a bug")
  a <- callDouble defaultTimeout () 3
  b <- callEcho defaultTimeout () "test"
  c <- callDoubleSum defaultTimeout () [1, 2, 3, 4]
  mapM_ cancel serverThreads
  return . describe "simple API" $
    it "should compute results correctly" $ (a, b, c) `shouldBe` (6, "test", 20)

doubleStream :: Int -> IO (Streamly.Serial Int)
doubleStream x = return . Streamly.fromList $ fmap (x *) [1, 2, 3]

echoStream :: Text -> IO (Streamly.Serial Text)
echoStream = return . Streamly.fromList . replicate 3

type StreamAPI = Protocol "double'" Int Int :<|> Protocol "echo'" Text Text

proxyStreamAPI :: Proxy StreamAPI
proxyStreamAPI = Proxy

handleStreamAPI :: Butler.Server' StreamAPI
handleStreamAPI = doubleStream :<|> echoStream

_testStreamAPI :: IO Spec
_testStreamAPI = do
  requestTable <- HashTable.new -- Route (MVar Text)
  resultTable <- HashTable.new -- Route (MVar Text)
  serverThreads <-
    Butler.makeServer'
      proxyStreamAPI
      handleStreamAPI
      (\route handler -> do
         requestVar <- MVar.newEmptyMVar
         resultsVar <- MVar.newEmptyMVar
         HashTable.insert
           (requestTable :: HashTable Route (MVar.MVar Text))
           route
           requestVar
         HashTable.insert
           (resultTable :: HashTable Route (MVar ( Text -> IO ()
                                                 , IO ()
                                                 , Streamly.Serial Text)))
           route
           resultsVar
         -- Stream server thread per route.
         serverThread <-
           async . forever $ do
             request <- MVar.takeMVar requestVar
             results <- handler request
             (push, close, _) <- MVar.takeMVar resultsVar
             Streamly.mapM_ push results
             close
         return serverThread)
  let callDouble' :<|> callEcho' =
        Butler.makeClient'
          proxyStreamAPI
          (\_ _ route request -> do
             requestVarMaybe <- HashTable.lookup requestTable route
             resultsVarMaybe <- HashTable.lookup resultTable route
             case (requestVarMaybe, resultsVarMaybe) of
               (Just requestVar, Just resultsVar) -> do
                 rp <- repeatableStream
                 let (_, _, stream) = rp
                 MVar.putMVar requestVar request
                 MVar.putMVar resultsVar rp
                 return stream
               _ -> panic "this is a bug")
  a <- callDouble' defaultTimeout () 3
  b <- callEcho' defaultTimeout () "test"
  as <- Streamly.toList a
  bs <- Streamly.toList b
  mapM_ cancel serverThreads
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
        Butler.makePublisher'
          proxyPublishAPI
          (\_ route message ->
             case route of
               Route "increment" -> pushInc message
               Route "repeat" -> pushRep message
               _ -> return ())
  let subscribeIncrement' :<|> subscribeRepeat' =
        Butler.makeSubscriber'
          proxyPublishAPI
          (\_ route -> do
             idInt <- Random.randomRIO (1, 10000)
             let id = Id . show $ (idInt :: Int)
             return $
               case route of
                 Route "increment" -> (id, streamInc)
                 Route "repeat" -> (id, streamRep)
                 _ -> (id, Streamly.nil))
  (_, resultsInc) <- subscribeIncrement' ()
  (_, resultsRep) <- subscribeRepeat' ()
  async $ do
    publishIncrement' () (Streamly.fromList [1, 2, 3, 4])
    closeInc
  async $ do
    publishRepeat' () (Streamly.fromList ["test", "test", "test"])
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
    -- TODO: Explicitly test Butler.run.
    -- (Currently tested in Bridge code).
