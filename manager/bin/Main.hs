import qualified Bridge.Transports.Amqp as Amqp
import qualified Bridge.Transports.WebSocket as WebSocket
import qualified Manager
import VestPrelude

main :: IO Void
main =
  withForever WebSocket.localConfig $ \serverTransport ->
    withForever Amqp.localConfig $ \clientTransport ->
      Manager.start serverTransport clientTransport clientTransport
