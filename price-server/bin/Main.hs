import qualified Bridge
import qualified PriceServer
import VestPrelude

priceServerConfig :: PriceServer.Config
priceServerConfig = PriceServer.Config {}

main :: IO ()
main = PriceServer.start Bridge.localConfig priceServerConfig
