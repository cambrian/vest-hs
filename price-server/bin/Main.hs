import qualified Bridge.Transports.Amqp as Amqp
import PriceServer
import VestPrelude

config :: Config
config = Config {}

main :: IO Void
main =
  withForever
    Amqp.localConfig
    (\amqp ->
       withForever
         (config, amqp)
         (start amqp amqp (Proxy :: Proxy PriceableCurrencies)))
