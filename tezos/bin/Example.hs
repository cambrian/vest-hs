import qualified Stream
import Tezos
import Vest
import Vest.Http

publicTezosConfig :: Config
publicTezosConfig =
  Config {schemeType = HttpsType, host = "rpc.tezrpc.me", port = 443, path = ""}

printMainHeadConstants :: T -> IO ()
printMainHeadConstants connection = do
  constants <-
    direct (request (Proxy :: Proxy BlockConstants) "main" "head") connection
  print constants

printBlocks :: T -> IO ()
printBlocks connection = do
  monitorStream <- streaming (request (Proxy :: Proxy MonitorBlocks)) connection
  Stream.mapM_ print monitorStream

main :: IO ()
main = do
  connection <- make publicTezosConfig
  printMainHeadConstants connection
  printBlocks connection
