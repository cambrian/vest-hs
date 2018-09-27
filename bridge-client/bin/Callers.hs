import BridgeClient
import qualified Manager
import VestPrelude

main :: IO ()
main = mapM_ print $ makeSpecTsTypes (Proxy :: Proxy Manager.ManagerApi)
