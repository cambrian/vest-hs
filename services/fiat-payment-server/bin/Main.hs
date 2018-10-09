import qualified Bridge.Transports.Amqp as Amqp
import FiatPaymentServer
import Vest

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
