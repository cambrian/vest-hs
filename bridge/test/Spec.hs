import qualified Bridge
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Function ((&))
import qualified Data.List
import qualified Streamly.Prelude as Streamly
import VestPrelude

bridgeConfig :: Bridge.Config
bridgeConfig =
  Bridge.Config
    { hostname = "localhost"
    , virtualHost = "/"
    , username = "guest"
    , password = "guest"
    }

testBasic :: Bridge.T -> IO ()
testBasic bridge = do
  let xs = [1, 2, 3]
  let route = Route "testBasic"
  Bridge.serveRPC' bridge route (\() -> Streamly.fromList xs)
  results <- Bridge.callRPC' @() @Int bridge route ()
  resultList <- Streamly.toList results
  when (resultList /= xs) $ panic "testBasic failed."
  putText "passed testBasic"

testMultipleFunctions :: Bridge.T -> IO ()
testMultipleFunctions bridge = do
  let nums = [1, 2, 3]
  let chars = ['a', 'b', 'c']
  let routeNums = Route "testMultipleFunctionsNums"
  let routeChars = Route "testMultipleFunctionsChars"
  Bridge.serveRPC' bridge routeNums (\() -> Streamly.fromList nums)
  Bridge.serveRPC' bridge routeChars (\() -> Streamly.fromList chars)
  resultNums <- Bridge.callRPC' @() @Int bridge routeNums ()
  resultChars <- Bridge.callRPC' @() @Char bridge routeChars ()
  resultNumList <- Streamly.toList resultNums
  resultCharList <- Streamly.toList resultChars
  when (resultNumList /= nums) $ panic "testMultipleFunctions failed."
  when (resultCharList /= chars) $ panic "testMultipleFunctions failed."
  putText "passed testMultipleFunctions"

testMultipleConsumption :: Bridge.T -> IO ()
testMultipleConsumption bridge = do
  let xs = [1, 2, 3]
  let route = Route "testMultipleConsumption'"
  Bridge.serveRPC' bridge route (\() -> Streamly.fromList xs)
  results <- Bridge.callRPC' @() @Int bridge route ()
  resultList <- Streamly.toList results
  resultList2 <- Streamly.toList results
  when (resultList /= xs) $ panic "testMultipleConsumption failed."
  when (resultList2 /= xs) $ panic "testMultipleConsumption failed."
  putText "passed testMultipleConsumption"

testConcurrent :: Bridge.T -> IO ()
testConcurrent bridge = do
  let route = Route "testConcurrent"
  Bridge.serveRPC'
    bridge
    route
    (\(x :: Int) ->
       Streamly.repeatM (threadDelay 100000 >> return x) & Streamly.take 3)
  results1 <- Bridge.callRPC' @Int @Int bridge route 1
  results2 <- Bridge.callRPC' @Int @Int bridge route 2
  resultList1 <- Streamly.toList results1
  resultList2 <- Streamly.toList results2
  unless (Data.List.all (\x -> x == 1) resultList1) $
    panic "testConcurrent failed."
  unless (Data.List.all (\x -> x == 2) resultList2) $
    panic "testConcurrent failed."
  putText "passed testConcurrent"

main :: IO ()
main = do
  bridge <- Bridge.make bridgeConfig
  testBasic bridge
  testMultipleFunctions bridge
  testMultipleConsumption bridge
  testConcurrent bridge
