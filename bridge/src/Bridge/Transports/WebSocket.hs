module Bridge.Transports.WebSocket
  ( T(..)
  , Config(..)
  , RequestMessage(..)
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
  { id :: Id "RpcRequest"
  , route :: Id "Rpc"
    -- Other metadata (time?).
  , reqText :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data ResponseMessage = ResponseMessage
  { requestId :: Id "RpcRequest"
    -- Other metadata.
  , response :: Either RpcClientException Text
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Config = Config
  { serverPort :: Port
  , pingInterval :: Int
  , clientUri :: Id "Uri"
  , clientPort :: Port
  , clientPath :: Id "Path"
  }

localConfig :: ResourceConfig T
localConfig =
  Config
    { serverPort = Port 3000
    , pingInterval = 30
    , clientUri = Id "127.0.0.1"
    , clientPort = Port 3000
    , clientPath = Id "/"
    }

data T = T
  { serverThread :: Async ()
  , clients :: HashTable (Id "Client") WS.Connection
  -- Value is request push stream.
  , servedRouteRequests :: HashTable (Id "Rpc") ( (Id "Client", RequestMessage) -> IO ()
                                                , IO ()
                                                , Streamly.Serial ( Id "Client"
                                                                  , RequestMessage))
  -- Value is response push stream.
  , servedConnectionResponses :: HashTable (Id "Client") ( Text -> IO ()
                                                         , IO ()
                                                         , Streamly.Serial Text)
  , clientUri :: Id "Uri"
  , clientPort :: Port
  , clientPath :: Id "Path"
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
    -- Wait for init to avoid race conditions.
    threadDelay $ sec 0.1 -- TODO: Make less jank.
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

connectClient :: T -> WS.Connection -> IO (Id "Client")
connectClient socketServer conn = do
  let T {clients} = socketServer
  clientId <- newUuid
  HashTable.insert clients clientId conn
  return clientId

disconnectClient :: T -> Id "Client" -> IO ()
disconnectClient T {clients, servedConnectionResponses} clientId = do
  HashTable.delete clients clientId
  HashTable.delete servedConnectionResponses clientId

serveClient :: T -> Id "Client" -> WS.Connection -> IO ()
serveClient T {servedRouteRequests, servedConnectionResponses} clientId conn = do
  responses <- nonRepeatablePushStream
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
         -- If the route is not served, swallow the request. TODO: Maybe error on this?
         requestsMaybe >|>| (\(pushIn, _, _) -> pushIn (clientId, reqMsg)))

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
       -- (publish -> res/Stream res -> IO ())
       -- Generic publisher on intermediate result x. Should encapsulate serializing the
       -- intermediate results.
    -> (Text -> Maybe req)
    -> Id "Rpc"
       -- Should throw if Route is already being served.
    -> T
    -> (req -> IO x)
    -> IO ()
    -- Should mutate t to store the details necessary for cleanup.
  _serve publisher deserialize route T { servedRouteRequests
                                       , servedConnectionResponses
                                       } handler = do
    (_, _, streamIn) <-
      HashTable.lookup servedRouteRequests route >>= \case
        Nothing -> do
          requests <- nonRepeatablePushStream
          HashTable.insert servedRouteRequests route requests
          return requests
        Just _ -> throw $ AlreadyServing route
    let handleMsg (clientId, RequestMessage {id = requestId, reqText}) = do
          let pub response = do
                outMaybe <- HashTable.lookup servedConnectionResponses clientId
                case outMaybe of
                  Nothing -> return () -- Rare but possible edge case. A connection might die while
                                       -- its request is being serviced, in which case we discard
                                       -- the response.
                  Just (pushOut, _, _) ->
                    void . pushOut . encode $
                    ResponseMessage {requestId, response}
          case deserialize reqText of
            Nothing -> pub . Left . BadCall $ "bad input: " <> reqText
            Just r -> handler r >>= publisher (pub . Right)
    -- Thread dies on its own when stream closed on kill.
    void . async $ Streamly.mapM_ handleMsg streamIn
  _call ::
       IO (res -> IO (), IO x, IO ())
       -- IO (push, result, done)
       -- Generic response handler that takes an intermediate result x and defines how to push it
       -- to the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Id "Rpc"
    -> T
    -> Time Second
       -- Timeout
    -> req
    -> IO x
    -- Runs a reader loop on the client connection, streaming results from the response pipe.
    -- Timeouts occur if a result or stream result is not received after the specified time.
  _call handler serialize deserializeUnsafe route T { clientUri
                                                    , clientPort
                                                    , clientPath
                                                    } _timeout req = do
    id <- newUuid
    let (Id _uri, Port _port, Id _path) = (clientUri, clientPort, clientPath)
    let (uriStr, pathStr) = (unpack _uri, unpack _path)
    let request = encode RequestMessage {id, route, reqText = serialize req}
    (push, result, waitForDone) <- handler
    (renewTimeout, timeoutDone) <- timeoutRenewable _timeout waitForDone
    -- TODO: Track these threads?
    mainThread <- myThreadId
    let wsClientApp conn = do
          readerThread <-
            async $ do
              let readLoop = do
                    msg <- WS.receiveData conn
                    case decode msg of
                      Nothing -> return () -- Swallow if entire message is garbled.
                      Just ResponseMessage {response} ->
                        case response of
                          Left exception -> throwTo mainThread exception
                          Right text ->
                            renewTimeout >> deserializeUnsafe text >>= push
                    readLoop
              readLoop
          WS.sendTextData conn request
          done <- timeoutDone
          cancel readerThread
          case done of
            Nothing -> throwTo mainThread (TimeoutException _timeout)
            Just () -> return ()
    WS.runClient uriStr _port pathStr wsClientApp
    result
