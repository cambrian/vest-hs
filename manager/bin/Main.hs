-- import qualified Bridge.Transports.Amqp as Amqp
import qualified Bridge.Transports.WebSocket as WebSocket
import qualified Manager
import VestPrelude

main :: IO Void
main =
  withForever WebSocket.localConfig $ \serverTransport ->
    withForever WebSocket.localConfig $ \clientTransport ->
      Manager.start serverTransport clientTransport clientTransport
      -- Should be AMQP client but the demo is not standalone if so.
