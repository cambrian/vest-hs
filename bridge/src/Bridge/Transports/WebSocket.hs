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
  , headers :: Headers -- Metadata.
  , route :: Text' "Rpc"
    -- Other metadata (time?).
  , reqText :: Text' "Request" -- Should be the serialization of a request object, but this is not
                                -- guaranteed.
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data ResponseMessage = ResponseMessage
  { requestId :: Text' "RequestId"
    -- Other metadata.
  , resText :: Text' "Response"
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Config = Config
  { serverPort :: Port
  , pingInterval :: Int
  , clientUri :: Text' "Uri"
  , clientPort :: Port
  , clientPath :: Text' "Path"
  }

localConfig :: Config
localConfig =
  Config
    { serverPort = Port 3000
    , pingInterval = 30
    , clientUri = Text' "127.0.0.1"
    , clientPort = Port 3000
    , clientPath = Text' "/"
    }

data T = T
  { serverThread :: Async ()
  , clients :: HashTable (Text' "ClientId") WS.Connection
  -- Value is request push stream.
  , servedRouteRequests :: HashTable (Text' "Rpc") ( ( Text' "ClientId"
                                                     , RequestMessage) -> IO ()
                                                   , IO ()
                                                   , Streamly.Serial ( Text' "ClientId"
                                                                     , RequestMessage))
  -- Value is response push stream.
  , servedConnectionResponses :: HashTable (Text' "ClientId") ( Text -> IO ()
                                                              , IO ()
                                                              , Streamly.Serial Text)
  , clientUri :: Text' "Uri"
  , clientPort :: Port
  , clientPath :: Text' "Path"
  }

type instance ResourceConfig T = Config

instance Resource T where
  hold :: ResourceConfig T -> IO T
  -- Connects to bridge, begins listening on client connections.
  hold Config {serverPort, pingInterval, clientUri, clientPort, clientPath} = do
    let Port _serverPort = serverPort
    clients <- HashTable.new
    servedRouteRequests <- HashTable.new
    servedConnectionResponses <- HashTable.new
    -- Warp runs forever on its own thread.
    socketServerVar <- newEmptyMVar
    serverThread <-
      async $ do
        socketServer <- readMVar socketServerVar
        -- Wait for thread ID to be added to server state.
        Warp.run _serverPort $
          WS.websocketsOr
            WS.defaultConnectionOptions
            (wsApp socketServer pingInterval)
            httpApp
    let socketServer =
          T
            { serverThread
            , clients
            , servedRouteRequests
            , servedConnectionResponses
            , clientUri
            , clientPort
            , clientPath
            }
    putMVar socketServerVar socketServer
    -- Wait for init to avoid test race conditions.
    threadDelay $ sec 0.1
    return socketServer
  release :: T -> IO ()
  release T {serverThread, servedRouteRequests, servedConnectionResponses} = do
    HashTable.mapM_ (\(_, (_, close, _)) -> close) servedRouteRequests
    HashTable.mapM_ (\(_, (_, close, _)) -> close) servedConnectionResponses
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
disconnectClient T {clients, servedConnectionResponses} clientId = do
  HashTable.delete clients clientId
  HashTable.delete servedConnectionResponses clientId

serveClient :: T -> Text' "ClientId" -> WS.Connection -> IO ()
serveClient T {servedRouteRequests, servedConnectionResponses} clientId conn = do
  responses <- singleUsePushStream
  HashTable.insert servedConnectionResponses clientId responses
  let (_, _, streamOut) = responses
  -- Thread dies on its own when the response is complete.
  async $ Streamly.mapM_ (WS.sendTextData conn) streamOut
  Monad.forever $ do
    msg <- WS.receiveData conn
    decode msg >|>| -- Do nothing if request message is malformed.
      (\reqMsg -> do
         let RequestMessage {route} = reqMsg
         requestsMaybe <- HashTable.lookup servedRouteRequests route
         -- If the route is not served, swallow the request.
         requestsMaybe >|>| (\(pushIn, _, _) -> pushIn (clientId, reqMsg)))

instance RpcTransport T where
  _serve ::
       ((Text' "Response" -> IO ()) -> Headers -> Text' "Request" -> IO ())
    -> Text' "Rpc"
    -> T
    -> IO ()
  _serve processor route T {servedRouteRequests, servedConnectionResponses} = do
    (_, _, streamIn) <-
      HashTable.lookup servedRouteRequests route >>= \case
        Nothing -> do
          requests <- singleUsePushStream
          HashTable.insert servedRouteRequests route requests
          return requests
        Just _ -> throw $ AlreadyServing route
    let handleMsg (clientId, RequestMessage {id = requestId, headers, reqText}) = do
          let send resText = do
                outMaybe <- HashTable.lookup servedConnectionResponses clientId
                case outMaybe of
                  Nothing -> return () -- Rare but possible edge case. A connection might die while
                                       -- its request is being serviced, in which case we discard
                                       -- the response.
                  Just (pushOut, _, _) ->
                    void . pushOut . encode $
                    ResponseMessage {requestId, resText}
          processor send headers reqText
    -- Thread dies on its own when stream closed on kill.
    void . async $ Streamly.mapM_ handleMsg streamIn
 -- Only for testing purposes (inefficient since it spawns a connection per call).
  _call ::
       ((Headers -> Text' "Request" -> IO ()) -> Time Second -> Headers -> req -> IO ( Text' "Response" -> IO ()
                                                                                     , IO x
                                                                                     , IO ()))
    -> Text' "Rpc"
    -> T
    -> Time Second
    -> Headers
    -> req
    -> IO x
  _call processor route T {clientUri, clientPort, clientPath} _timeout headers req = do
    id <- newUUID
    let (Text' _uri, Port _port, Text' _path) =
          (clientUri, clientPort, clientPath)
    let (uriStr, pathStr) = (unpack _uri, unpack _path)
    let send conn _headers reqText = do
          WS.sendTextData conn $
            encode RequestMessage {id, headers = _headers, route, reqText}
    resultMVar <- newEmptyMVar
    -- TODO: Track these threads?
    let wsClientApp conn = do
          (push, _result, waitForDone) <-
            processor (send conn) _timeout headers req
          readerThread <-
            async $ do
              let readLoop = do
                    msg <- WS.receiveData conn
                    case decode msg of
                      Nothing -> return () -- Swallow if entire message is garbled.
                      Just ResponseMessage {resText} -> push resText
                    readLoop
              readLoop
          putMVar resultMVar _result
          waitForDone >> cancel readerThread
    WS.runClient uriStr _port pathStr wsClientApp
    result <- takeMVar resultMVar
    result
