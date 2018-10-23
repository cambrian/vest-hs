import Http
import qualified Stream
import Test
import Tezos
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

printMainBlock :: Text -> T -> IO ()
printMainBlock block connection =
  direct
    (request (Proxy :: Proxy GetBlockOperations) mainChain block)
    connection >>=
  (print .
   filter
     (\case
        Delegation _ -> True
        Transaction _ -> True
        Origination _ -> True
        _ -> False) .
   concatMap contents . concat)

printMonitorBlocks :: T -> IO ()
printMonitorBlocks connection = do
  monitorStream <- streaming (request (Proxy :: Proxy MonitorBlocks)) connection
  Stream.mapM_ print monitorStream

main :: IO ()
main = do
  connection <- make publicTezosConfig
  ignoreIO $ printMainHeadConstants connection
  ignoreIO $ printMainHeadContractsCount connection
  ignoreIO $
    printContractManager "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k" connection
  ignoreIO $
    printContractBalance "KT1Xnjog1ou1HNHQNsD9nVi3ddkb9YQ5f28k" connection
  ignoreIO $
    printFrozenBalanceCycles "tz1Zhv3RkfU2pHrmaiDyxp7kFZpZrUCu1CiF" connection
  ignoreIO $
    printDelegatedContracts "tz1Zhv3RkfU2pHrmaiDyxp7kFZpZrUCu1CiF" connection
  ignoreIO $
    printMainBlock
      "BLpySJC2wRULnED2Y8nPkH7nUzASeorvft1iBMC2KhQbD79rw7r"
      connection
  getCycleRewardInfo
    connection
    (Tagged "BMA132SHdAJ9jueDF7BazcQoSsU28kV3gvusZsDVi3HvPkUHC4y")
    (Tagged "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN") >>=
    print
  ignoreIO $ printMonitorBlocks connection
