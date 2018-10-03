import qualified Bridge.Transports.WebSocket as WebSocket
import qualified DummyManager
import VestPrelude

main :: IO Void
main =
  withForever WebSocket.localConfig $ \serverTransport ->
    withForever WebSocket.localConfig $ \clientTransport ->
      DummyManager.start serverTransport clientTransport clientTransport
