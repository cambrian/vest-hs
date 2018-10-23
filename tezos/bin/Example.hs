import Http
import qualified Stream
import Tezos.Node
import Vest

publicTezosConfig :: ResourceConfig T
publicTezosConfig =
  Config {schemeType = HttpsType, host = "rpc.tezrpc.me", port = 443, path = ""}

printMainHeadConstants :: T -> IO ()
printMainHeadConstants connection =
  direct (request (Proxy :: Proxy GetConstants) mainChain headBlock) connection >>=
  print

printMainHeadContractsCount :: T -> IO ()
printMainHeadContractsCount connection =
  direct (request (Proxy :: Proxy ListContracts) mainChain headBlock) connection >>=
  (print . length)

printContractManager :: Text -> T -> IO ()
printContractManager contract connection =
  direct
    (request (Proxy :: Proxy GetContractManager) mainChain headBlock contract)
    connection >>=
  print

printContractBalance :: Text -> T -> IO ()
printContractBalance contract connection =
  direct
    (request (Proxy :: Proxy GetContractBalance) mainChain headBlock contract)
    connection >>=
  print

printFrozenBalanceCycles :: Text -> T -> IO ()
printFrozenBalanceCycles delegate connection =
  direct
    (request
       (Proxy :: Proxy ListFrozenBalanceCycles)
       mainChain
       headBlock
       delegate)
    connection >>=
  print

printDelegatedContracts :: Text -> T -> IO ()
printDelegatedContracts delegate connection =
  direct
    (request
       (Proxy :: Proxy ListDelegatedContracts)
       mainChain
       headBlock
       delegate)
    connection >>=
  print

printMainHead :: T -> IO ()
printMainHead connection =
  direct (request (Proxy :: Proxy GetBlock) mainChain headBlock) connection >>=
  print

printMonitorBlocks :: T -> IO ()
printMonitorBlocks connection = do
  monitorStream <- streaming (request (Proxy :: Proxy MonitorBlocks)) connection
  Stream.mapM_ print monitorStream

main :: IO ()
main = do
  connection <- make publicTezosConfig
  witness $ printMainHeadConstants connection
  witness $ printMainHeadContractsCount connection
  witness $
    printContractManager "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k" connection
  witness $
    printContractBalance "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k" connection
  witness $
    printFrozenBalanceCycles "tz1Zhv3RkfU2pHrmaiDyxp7kFZpZrUCu1CiF" connection
  witness $
    printDelegatedContracts "tz1Zhv3RkfU2pHrmaiDyxp7kFZpZrUCu1CiF" connection
  printMainHead connection
  witness $ printMonitorBlocks connection
