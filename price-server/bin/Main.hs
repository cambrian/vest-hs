import qualified Bridge
import qualified PriceServer
import VestPrelude

priceServerConfig :: PriceServer.Config
priceServerConfig = PriceServer.Config {bridgeConfig = Bridge.localConfig}

main :: IO ()
main = PriceServer.start priceServerConfig
