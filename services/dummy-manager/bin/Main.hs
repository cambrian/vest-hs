import Bridge
import qualified Bridge.Transports.WebSocket as WebSocket
import DummyManager
import Vest

main :: IO Void
main =
  start
    @T
    (\service -> do
       serve
         handlers
         service
         (Proxy :: Proxy (Api, WebSocket.T))
         (webSocket service)
       blockForever)
