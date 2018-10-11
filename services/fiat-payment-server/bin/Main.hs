import FiatPaymentServer
import Vest
import qualified Vest.Bridge.Transports.Amqp as Amqp

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
