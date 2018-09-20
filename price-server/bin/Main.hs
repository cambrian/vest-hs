import qualified Bridge.Transports.Amqp as Amqp
import qualified PriceServer
import VestPrelude

config :: PriceServer.Config
config = PriceServer.Config {}

main :: IO ()
main =
  withForever
    Amqp.localConfig
    (\amqp -> do
       t <- PriceServer.make config amqp
       PriceServer.start (Proxy :: Proxy (SymbolT "XTZ")) t amqp amqp)
