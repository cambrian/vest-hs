module EndpointTest
  ( main
  ) where

import TezosPlatformStats
import Vest
import qualified WebSocket

webSocketConfig :: ResourceConfig WebSocket.T
webSocketConfig =
  WebSocket.Config
    { WebSocket.servers =
        [ ( Tagged $ namespace @TezosPlatformStats.T
          , WebSocket.ServerInfo
              {uri = Tagged "127.0.0.1", port = Tagged 3000, path = Tagged "/"})
        ]
    , WebSocket.servePort = Tagged 3000
    , WebSocket.pingInterval = 30
    }

main :: IO ()
main = do
  with webSocketConfig $ \websocket -> do
    _ <- testOriginationsByImplicitAddressEndpoint websocket
    _ <- testBakerCountEndpoint websocket
    return ()
  return ()
