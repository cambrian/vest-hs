import qualified Stream
import Tezos
import Tezos.Node
import Vest
import Vest.Http

publicTezosConfig :: ResourceConfig T
publicTezosConfig =
  Config {schemeType = HttpsType, host = "rpc.tezrpc.me", port = 443, path = ""}

publicTzScanConfig :: ResourceConfig T
publicTzScanConfig =
  Config
    {schemeType = HttpsType, host = "api5.tzscan.io", port = 443, path = ""}

printMainHeadContractsCount :: T -> IO ()
printMainHeadContractsCount connection =
  direct (request (Proxy :: Proxy ListContracts) mainChain headBlock) connection >>=
  (print . length)

printContractManager :: T -> Text -> IO ()
printContractManager connection contract =
  direct
    (request (Proxy :: Proxy GetContractManager) mainChain headBlock contract)
    connection >>=
  print

printMainHeadConstants :: T -> IO ()
printMainHeadConstants connection =
  direct (request (Proxy :: Proxy GetConstants) mainChain headBlock) connection >>=
  print

printBlocks :: T -> IO ()
printBlocks connection = do
  monitorStream <- streaming (request (Proxy :: Proxy MonitorBlocks)) connection
  Stream.mapM_ print monitorStream

main :: IO ()
main = do
  connection <- make publicTezosConfig
  connectionTzScan <- make publicTzScanConfig
  getRewardSplit
    connectionTzScan
    (Tagged "tz1Zhv3RkfU2pHrmaiDyxp7kFZpZrUCu1CiF")
    36 >>=
    print . length . delegations
  printMainHeadContractsCount connection
  printContractManager connection "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k"
  printMainHeadConstants connection
  printBlocks connection
