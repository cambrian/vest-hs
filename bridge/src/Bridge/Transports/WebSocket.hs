module Bridge.Transports.WebSocket
  ( T(..)
  , Config(..)
  , ServerInfo(..)
  , RequestMessage(..)
  , ResponseMessage(..)
  , localConfig
  ) where

import Bridge.Rpc
import qualified Control.Exception as Exception
import qualified Data.HashTable.IO as HashTable
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

-- WebSockets don't exactly fit into the bridge model, since WebSocket connections are between
-- clients and servers, rather than through a messaging service. That said, we provide a WebSocket
-- transport so that components can serve public WebSocket endpoints to e.g. browsers.
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

data ServerInfo = ServerInfo
  { uri :: Text' "Uri"
  , port :: Int' "Port"
  , path :: Text' "Path"
  , routes :: [Text' "Route"] -- ^ Not type safe.
  }

data Config = Config
  { servePort :: Int' "Port"
  , pingInterval :: Int
  , servers :: [ServerInfo]
  }

localConfig :: Config
localConfig = Config {servePort = Tagged 3000, pingInterval = 30, servers = []}

data WebSocketClientException =
  NoServerForRoute (Text' "Route")
  deriving (Show, Exception)

data T = T
  { serverRequestHandlers :: HashTable (Text' "Route") (Text' "ClientId" -> RequestMessage -> IO (Async ()))
  -- ^ For a server, requests need to be aggregated by route.
  , serverResponseHandlers :: HashTable (Text' "ClientId") (ResponseMessage -> IO ())
  -- ^ For a server, stores each client's response queue.
  , clientRequestHandlers :: HashTable (Text' "Route") (RequestMessage -> IO ())
  , clientResponseHandlers :: HashTable (Text' "RequestId") (Text' "Response" -> IO ())
  , serverThread :: Async ()
  , clientThreads :: [Async ()]
  -- ^ Should clean themselves up when canceled
  }

type instance ResourceConfig T = Config

instance Resource T where
  make :: ResourceConfig T -> IO T
  -- Begins listening on servePort, connects to each server and begins listening for responses.
  make Config {servePort, pingInterval, servers} = do
    serverRequestHandlers <- HashTable.new
    serverResponseHandlers <- HashTable.new
    clientRequestHandlers <- HashTable.new
    clientResponseHandlers <- HashTable.new
    -- Warp.run blocks forever, so we put it in its own thread.
    serverThread <-
      async $
      Warp.run (untag servePort) $
      WS.websocketsOr
        WS.defaultConnectionOptions
        (wsServe serverRequestHandlers serverResponseHandlers pingInterval)
        noHttpApp
    clientThreads <-
      forM
        servers
        (\ServerInfo {uri, port, path, routes} -> do
           requestMVar <- newEmptyMVar
           forM_
             routes
             (\route ->
                HashTable.insert
                  clientRequestHandlers
                  route
                  (putMVar requestMVar))
           async $
             WS.runClient
               (unpack $ untag uri)
               (untag port)
               (unpack $ untag path)
               (wsClientApp clientResponseHandlers requestMVar))
    return
      T
        { serverRequestHandlers
        , serverResponseHandlers
        , clientRequestHandlers
        , clientResponseHandlers
        , serverThread
        , clientThreads
        }
  cleanup :: T -> IO ()
  cleanup T {serverThread, clientThreads} = do
    cancel serverThread
    mapM_ cancel clientThreads

noHttpApp :: Wai.Application
noHttpApp _ respond =
  respond $
  Wai.responseLBS
    Http.status400
    []
    "HTTP requests not supported (use WebSockets)."

-- Each wsServe is per client and runs on its own thread.
wsServe ::
     HashTable (Text' "Route") (Text' "ClientId" -> RequestMessage -> IO (Async ()))
  -> HashTable (Text' "ClientId") (ResponseMessage -> IO ())
  -> Int
  -> WS.ServerApp
wsServe serverRequestHandlers serverResponseHandlers pingInterval pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- newUUID
  HashTable.insert
    serverResponseHandlers
    clientId
    (WS.sendTextData conn . serialize @'JSON)
  WS.forkPingThread conn pingInterval
  Exception.finally
    (serveClient serverRequestHandlers clientId conn)
    (HashTable.delete serverResponseHandlers clientId)

serveClient ::
     HashTable (Text' "Route") (Text' "ClientId" -> RequestMessage -> IO (Async ()))
  -> Text' "ClientId"
  -> WS.Connection
  -> IO ()
serveClient serverRequestHandlers clientId conn =
  forever $ do
    msg <- WS.receiveData conn
    deserialize @'JSON msg >|>| -- Do nothing if request message does not deserialize.
      (\reqMsg -> do
         let RequestMessage {route} = reqMsg
         maybeHandler <- HashTable.lookup serverRequestHandlers route
         -- If the route is not served, swallow the request.
         maybeHandler >|>| (\h -> h clientId reqMsg))

wsClientApp ::
     HashTable (Text' "RequestId") (Text' "Response" -> IO ())
  -> MVar RequestMessage
  -> WS.ClientApp ()
wsClientApp clientResponseHandlers requestMVar conn =
  withAsync
    (async . forever $ do
       msg <- WS.receiveData conn
       case deserialize @'JSON msg of
         Nothing -> return () -- Swallow if entire message is garbled.
         Just ResponseMessage {requestId, resText} -> do
           maybeHandler <- HashTable.lookup clientResponseHandlers requestId
            -- If the route is not served, swallow the request.
           maybeHandler >|>| ($ resText))
    (const . forever $ do
       request <- takeMVar requestMVar
       WS.sendTextData conn (serialize @'JSON request))

instance RpcTransport T where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> Text' "Route"
    -> T
    -> IO ()
  _consumeRequests asyncHandler route T { serverRequestHandlers
                                        , serverResponseHandlers
                                        } =
    HashTable.lookup serverRequestHandlers route >>= \case
      Just _ -> throw $ AlreadyServing route
      Nothing -> do
        print route
        HashTable.insert serverRequestHandlers route handleMsg
    where
      handleMsg clientId RequestMessage {id = requestId, headers, reqText} =
        HashTable.lookup serverResponseHandlers clientId >>= \case
          Nothing -> async $ return ()
          Just handler ->
            let respond resText = handler ResponseMessage {requestId, resText}
             in asyncHandler headers reqText respond
  _issueRequest ::
       (Text' "Response" -> IO ())
    -> Text' "Route"
    -> T
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())
  _issueRequest respond route T {clientRequestHandlers, clientResponseHandlers} headers reqText = do
    id <- newUUID
    makeRequest <-
      HashTable.lookup clientRequestHandlers route >>=
      fromJustUnsafe (NoServerForRoute route)
    HashTable.insert clientResponseHandlers id respond
    makeRequest $ RequestMessage {id, headers, route, reqText}
    return $ Tagged $ HashTable.delete clientResponseHandlers id
