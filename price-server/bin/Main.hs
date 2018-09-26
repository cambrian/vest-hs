import qualified Bridge.Transports.Amqp as Amqp
import PriceServer
import VestPrelude

config :: Config
config = Config {}

amqpConfig :: Amqp.Config
amqpConfig = Amqp.localConfig

main :: IO Void
main =
  withForever
    amqpConfig
    (\amqp ->
       make config amqp >>= start amqp amqp (Proxy :: Proxy PriceableCurrencies))
