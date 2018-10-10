import Bridge
import qualified Bridge.Transports.WebSocket as WebSocket
import DummyManager
import Vest

main :: IO Void
main = start @T (const . return $ ()) handlers
