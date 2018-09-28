module Bridge.Transports.WebSocket
  ( T(..)
  , Config(..)
  , RequestMessage(..)
  , ResponseMessage(..)
  , localConfig
  ) where

import Bridge.Rpc
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.HashTable.IO as HashTable
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: Text' "RequestId"
  , headers :: Headers
  , route :: Text' "Route"
  , reqText :: Text' "Request"
  } deriving (Generic, FromJSON, ToJSON)

data ResponseMessage = ResponseMessage
  { requestId :: Text' "RequestId"
  , resText :: Text' "Response"
  } deriving (Generic, FromJSON, ToJSON)

data Config = Config
  { serverPort :: Int' "Port"
  , pingInterval :: Int
  }

localConfig :: Config
localConfig =
  Config
    { serverPort = Tagged 3000
    , pingInterval = 30
    }

data T = T
  { serverThread :: Async ()
  , clients :: HashTable (Text' "ClientId") WS.Connection
  , serverRequestHandlers :: HashTable (Text' "Route") (Text' "ClientId" -> RequestMessage -> IO (Async ()))
  -- ^ For a server, requests need to be aggregated by route.
  , serverResponseHandlers :: HashTable (Text' "ClientId") (ResponseMessage -> IO ())
  -- ^ For a server, stores each client's response queue
  -- We could also have a map for client response handlers, but since we don't expect Haskell
  -- websocket clients except for testing, we're not including it here.
  }

data TestClient = TestClient
  { serverUri :: Text' "Uri"
  , serverPort :: Int' "Port"
  , serverPath :: Text' "Path"
  }

localTestClient :: TestClient
localTestClient = TestClient
 { serverUri = Tagged "127.0.0.1"
 , serverPort = Tagged 3000
 , serverPath = Tagged "/"
 }

type instance ResourceConfig T = Config

instance Resource T where
  hold :: ResourceConfig T -> IO T
  -- Connects to bridge, begins listening on client connections.
  hold Config {serverPort, pingInterval} = do
    clients <- HashTable.new
    serverRequestHandlers <- HashTable.new
    serverResponseHandlers <- HashTable.new
    -- Warp runs forever on its own thread.
    socketServerVar <- newEmptyMVar
    serverThread <-
      async $ do
        socketServer <- readMVar socketServerVar
        -- Wait for thread ID to be added to server state.
        Warp.run (untag serverPort) $
          WS.websocketsOr
            WS.defaultConnectionOptions
            (wsApp socketServer pingInterval)
            httpApp
    let socketServer =
          T
            { serverThread
            , clients
            , serverRequestHandlers
            , serverResponseHandlers
            }
    putMVar socketServerVar socketServer
    -- Wait for init to avoid test race conditions.
    threadDelay $ sec 0.1
    return socketServer
  release :: T -> IO ()
  release T {serverThread} = do
    cancel serverThread

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "not a WebSocket request"

wsApp :: T -> Int -> WS.ServerApp
wsApp socketServer pingInterval pendingConn = do
  conn <- WS.acceptRequest pendingConn
  -- Each connection is on a new thread.
  clientId <- connectClient socketServer conn
  WS.forkPingThread conn pingInterval
  Exception.finally
    (serveClient socketServer clientId conn)
    (disconnectClient socketServer clientId)

connectClient :: T -> WS.Connection -> IO (Text' "ClientId")
connectClient socketServer conn = do
  let T {clients} = socketServer
  clientId <- newUUID
  HashTable.insert clients clientId conn
  return clientId

disconnectClient :: T -> Text' "ClientId" -> IO ()
disconnectClient T {clients, serverResponseHandlers} clientId = do
  HashTable.delete clients clientId
  HashTable.delete serverResponseHandlers clientId

serveClient :: T -> Text' "ClientId" -> WS.Connection -> IO ()
serveClient T {serverRequestHandlers, serverResponseHandlers} clientId conn = do
  let respond resMsg = WS.sendTextData conn (serialize @'JSON resMsg)
  HashTable.insert serverResponseHandlers clientId respond
  forever $ do
    msg <- WS.receiveData conn
    deserialize @'JSON msg >|>| -- Do nothing if request message does not deserialize.
      (\reqMsg -> do
         let RequestMessage {route} = reqMsg
         maybeHandler <- HashTable.lookup serverRequestHandlers route
         -- If the route is not served, swallow the request.
         maybeHandler >|>| (\h -> h clientId reqMsg))

instance RpcServerTransport T where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> Text' "Route"
    -> T
    -> IO ()
  _consumeRequests asyncHandler route T {serverRequestHandlers, serverResponseHandlers} = do
    HashTable.lookup serverRequestHandlers route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let handleMsg clientId RequestMessage {id = requestId, headers, reqText} = do
          HashTable.lookup serverResponseHandlers clientId >>= \case
            Nothing -> async $ return ()
            Just handler ->
              let respond resText = handler ResponseMessage {requestId, resText}
               in asyncHandler headers reqText respond
    HashTable.insert serverRequestHandlers route handleMsg

instance RpcClientTransport TestClient where
  _issueRequest ::
       (Text' "Response" -> IO ())
    -> Text' "Route"
    -> TestClient
    -> Headers
    -> Text' "Request"
    -> IO (IO ())
  -- ^ Only for testing purposes (inefficient since it spawns a connection per call).
  _issueRequest respond route TestClient {serverUri, serverPort, serverPath} headers reqText = do
    let rawUri = unpack $ untag serverUri
        rawPath = unpack $ untag serverPath
        rawPort = untag serverPort
        wsClientApp conn = do
          readerThread <-
            async $ do
              let readLoop = do
                    msg <- WS.receiveData conn
                    case deserialize @'JSON msg of
                      Nothing -> return () -- Swallow if entire message is garbled.
                      Just ResponseMessage {resText} -> respond resText
                    readLoop
              readLoop
          WS.sendTextData conn $
            serialize
              @'JSON
              RequestMessage {id=Tagged "", headers, route, reqText}
          return $ cancel readerThread
    WS.runClient rawUri rawPort rawPath wsClientApp
