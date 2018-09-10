import qualified SocketServer
import VestPrelude

-- import Control.Concurrent (threadDelay)
-- import Control.Monad (unless, when)
-- import Data.Function ((&))
-- import qualified Data.List
-- import qualified Streamly
-- import qualified Streamly.Prelude as Streamly
-- bridgeConfig :: Bridge.Config
-- bridgeConfig =
--   Bridge.Config
--     { hostname = "localhost"
--     , virtualHost = "/"
--     , username = "guest"
--     , password = "guest"
--     }
-- testBasic :: Bridge.T -> IO ()
-- testBasic bridge = do
--   let xs = [1, 2, 3]
--   Bridge.serveRPC bridge "testBasic" (\() -> Streamly.fromList xs)
--   results <- Bridge.callRPC @() @Int bridge "testBasic" ()
--   resultList <- Streamly.toList results
--   when (resultList /= xs) $ error "testBasic failed."
--   putStrLn "passed testBasic"
-- testMultipleFunctions :: Bridge.T -> IO ()
-- testMultipleFunctions bridge = do
--   let nums = [1, 2, 3]
--   let chars = ['a', 'b', 'c']
--   Bridge.serveRPC
--     bridge
--     "testMultipleFunctionsNums"
--     (\() -> Streamly.fromList nums)
--   Bridge.serveRPC
--     bridge
--     "testMultipleFunctionsChars"
--     (\() -> Streamly.fromList chars)
--   resultNums <- Bridge.callRPC @() @Int bridge "testMultipleFunctionsNums" ()
--   resultChars <- Bridge.callRPC @() @Char bridge "testMultipleFunctionsChars" ()
--   resultNumList <- Streamly.toList resultNums
--   resultCharList <- Streamly.toList resultChars
--   when (resultNumList /= nums) $ error "testMultipleFunctions failed."
--   when (resultCharList /= chars) $ error "testMultipleFunctions failed."
--   putStrLn "passed testMultipleFunctions"
-- testMultipleConsumption :: Bridge.T -> IO ()
-- testMultipleConsumption bridge = do
--   let xs = [1, 2, 3]
--   Bridge.serveRPC bridge "testMultipleConsumption" (\() -> Streamly.fromList xs)
--   results <- Bridge.callRPC @() @Int bridge "testMultipleConsumption" ()
--   resultList <- Streamly.toList results
--   resultList2 <- Streamly.toList results
--   when (resultList /= xs) $ error "testMultipleConsumption failed."
--   when (resultList2 /= xs) $ error "testMultipleConsumption failed."
--   putStrLn "passed testMultipleConsumption"
-- testConcurrent :: Bridge.T -> IO ()
-- testConcurrent bridge = do
--   Bridge.serveRPC
--     bridge
--     "testConcurrent"
--     (\(x :: Int) ->
--        Streamly.repeatM (threadDelay 100000 >> return x) & Streamly.take 3)
--   results1 <- Bridge.callRPC @Int @Int bridge "testConcurrent" 1
--   results2 <- Bridge.callRPC @Int @Int bridge "testConcurrent" 2
--   resultList1 <- Streamly.toList results1
--   resultList2 <- Streamly.toList results2
--   unless (Data.List.all (\x -> x == 1) resultList1) $
--     error "testConcurrent failed."
--   unless (Data.List.all (\x -> x == 2) resultList2) $
--     error "testConcurrent failed."
--   putStrLn "passed testConcurrent"
main :: IO ()
main = do
  socketServer <- SocketServer.makeServer 3000
  let socketClient = SocketServer.makeClient "localhost" 3000 "/"
  -- testBasic bridge
  -- testMultipleFunctions bridge
  -- testMultipleConsumption bridge
  -- testConcurrent bridge
  return ()
