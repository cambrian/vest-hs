import qualified Stream
import Tezos
import Vest
import Vest.Http

publicTezosConfig :: Config
publicTezosConfig =
  Config {schemeType = HttpsType, host = "rpc.tezrpc.me", port = 443, path = ""}

printMainHeadContractsCount :: T -> IO ()
printMainHeadContractsCount connection = do
  contracts <-
    direct
      (request (Proxy :: Proxy ListContracts) mainChain headBlock)
      connection
  print $ length contracts

printContractManager :: T -> Text -> IO ()
printContractManager connection contract = do
  manager <-
    direct
      (request (Proxy :: Proxy GetContractManager) mainChain headBlock contract)
      connection
  print manager

printMainHeadConstants :: T -> IO ()
printMainHeadConstants connection = do
  constants <-
    direct
      (request (Proxy :: Proxy GetConstants) mainChain headBlock)
      connection
  print constants

printBlocks :: T -> IO ()
printBlocks connection = do
  monitorStream <- streaming (request (Proxy :: Proxy MonitorBlocks)) connection
  Stream.mapM_ print monitorStream

main :: IO ()
main = do
  connection <- make publicTezosConfig
  printMainHeadContractsCount connection
  printContractManager connection "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k"
  printMainHeadConstants connection
  printBlocks connection
