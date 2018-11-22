module Transport.WebSocket
  ( T(..)
  , Config(..)
  , ServerInfo(..)
  , RequestMessage(..)
  , ResponseMessage(..)
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Data.HashTable.IO as HashTable
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Vest

-- WebSockets don't exactly fit into the Vest.Bridge model, since WebSocket connections are per
-- client/server and not via a messaging service. That said, we provide a WebSocket transport so
-- that components can serve public WebSocket endpoints to e.g. browsers.
data RequestMessage = RequestMessage
  { id :: UUID' "Request"
  , headers :: Headers
  , route :: Route
  , reqText :: Text' "Request"
  } deriving (Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''RequestMessage)

data ResponseMessage = ResponseMessage
  { requestId :: UUID' "Request"
  , resText :: Text' "Response"
  } deriving (Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''ResponseMessage)

data ServerInfo = ServerInfo
  { uri :: Text' "Uri"
  , port :: Word16' "Port"
  , path :: Text' "Path"
  } deriving (Generic, FromJSON)

data Config = Config
  { servePort :: Word16' "Port"
  , pingInterval :: Int
  , servers :: [(Text' "Server", ServerInfo)]
  } deriving (Generic, FromJSON)

-- Note: The "localhost" literal does not work with wsClient.
isLocalHost :: (Eq a, IsString a) => a -> Bool
isLocalHost x = x == "localhost" || x == "127.0.0.1"

newtype NoServerForNamespaceException =
  NoServerForNamespace (Text' "Server")
  deriving (Eq, Show)
  deriving anyclass (Exception)

data T = T
  { serverRequestHandlers :: HashTable Route (UUID' "Client" -> RequestMessage -> IO (Async ()))
  -- ^ For a server, requests need to be aggregated by route.
  , serverResponseHandlers :: HashTable (UUID' "Client") (ResponseMessage -> IO ())
  -- ^ For a server, stores each client's response queue.
  , clientRequestHandlers :: HashTable (Text' "Server") (RequestMessage -> IO ())
  , clientResponseHandlers :: HashTable (UUID' "Request") (Text' "Response" -> IO ())
  , serverThread :: Async' "ServerThread" ()
  , clientThreads :: [Async' "ClientThread" ()]
  -- ^ Should clean themselves up when canceled
  }

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
  -- Begins listening on servePort, connects to each server and begins listening for responses.
  make Config {servePort, pingInterval, servers} = do
    serverRequestHandlers <- HashTable.new
    serverResponseHandlers <- HashTable.new
    clientRequestHandlers <- HashTable.new
    clientResponseHandlers <- HashTable.new
    -- Warp.run blocks forever, so we put it in its own thread.
    serverThread <-
      asyncThrows' $
      Warp.run (fromIntegral $ untag servePort) $
      WS.websocketsOr
        WS.defaultConnectionOptions
        (wsServe serverRequestHandlers serverResponseHandlers pingInterval)
        noHttpApp
    clientThreads <-
      forM servers $ \(namespace, ServerInfo {uri, port, path}) -> do
        requestMVar <- newEmptyMVar
        HashTable.insert clientRequestHandlers namespace (putMVar requestMVar)
        -- Swallowing client exceptions is fine (usually just ConnectionClosed).
        async' $ do
          when (isLocalHost uri) (threadDelay (sec 0.05))
          -- ^ If this client is connecting to the local machine, wait for the server to start.
          WS.runClient
            (unpack $ untag uri)
            (fromIntegral $ untag port)
            (unpack $ untag path)
            (wsClientApp clientResponseHandlers requestMVar)
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
    cancel $ untag serverThread
    mapM_ (cancel . untag) clientThreads

noHttpApp :: Wai.Application
noHttpApp _ respond =
  respond $
  Wai.responseLBS
    Http.status400
    []
    "HTTP requests not supported (use WebSockets)."

-- Each wsServe is per client and runs on its own thread.
wsServe ::
     HashTable Route (UUID' "Client" -> RequestMessage -> IO (Async ()))
  -> HashTable (UUID' "Client") (ResponseMessage -> IO ())
  -> Int
  -> WS.ServerApp
wsServe serverRequestHandlers serverResponseHandlers pingInterval pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- nextUUID'
  HashTable.insert
    serverResponseHandlers
    clientId
    (WS.sendTextData conn . serialize @'JSON)
  WS.forkPingThread conn pingInterval
  finally
    (serveClient serverRequestHandlers clientId conn)
    (HashTable.delete serverResponseHandlers clientId)

serveClient ::
     HashTable Route (UUID' "Client" -> RequestMessage -> IO (Async ()))
  -> UUID' "Client"
  -> WS.Connection
  -> IO () -- Can this be IO Void?
-- ^ If the message fails to parse or the route is not served, swallows the request.
serveClient serverRequestHandlers clientId conn =
  forever . runMaybeT $ do
    reqMsg <- MaybeT $ WS.receiveData conn >>- deserialize @'JSON
    handler <- MaybeT $ HashTable.lookup serverRequestHandlers $ route reqMsg
    liftIO $ handler clientId reqMsg

wsClientApp ::
     HashTable (UUID' "Request") (Text' "Response" -> IO ())
  -> MVar RequestMessage
  -> WS.ClientApp ()
wsClientApp clientResponseHandlers requestMVar conn = do
  void . asyncThrows . forever . runMaybeT $ do
    ResponseMessage {requestId, resText} <-
      MaybeT $ WS.receiveData conn >>- deserialize @'JSON
    handler <- MaybeT $ HashTable.lookup clientResponseHandlers requestId
    liftIO $ handler resText
  forever $ do
    request <- takeMVar requestMVar
    WS.sendTextData conn (serialize @'JSON request)

instance RpcTransport T where
  serveRaw ::
       T
    -> Route
    -> (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> IO ()
  serveRaw T {serverRequestHandlers, serverResponseHandlers} route asyncHandler =
    HashTable.lookup serverRequestHandlers route >>= \case
      Just _ -> throw $ AlreadyServingException route
      Nothing -> HashTable.insert serverRequestHandlers route handleMsg
    where
      handleMsg clientId RequestMessage {id = requestId, headers, reqText} =
        asyncThrows . void . runMaybeT $ do
          handler <- MaybeT $ HashTable.lookup serverResponseHandlers clientId
          lift $
            asyncHandler
              headers
              reqText
              (\resText -> handler ResponseMessage {requestId, resText})
  callRaw ::
       T
    -> Route
    -> Headers
    -> Text' "Request"
    -> (Text' "Response" -> IO ())
    -> IO (IO' "Cleanup" ())
  callRaw T {clientRequestHandlers, clientResponseHandlers} route headers reqText respond = do
    namespace <-
      deserializeUnsafe' @'Pretty @(Namespaced "Server" Text) route >>-
      getNamespace'
    id <- nextUUID'
    makeRequest <-
      HashTable.lookup clientRequestHandlers namespace >>=
      fromJustUnsafe (NoServerForNamespace namespace)
    HashTable.insert clientResponseHandlers id respond
    makeRequest $ RequestMessage {id, headers, route, reqText}
    return $ Tagged $ HashTable.delete clientResponseHandlers id

instance HasRpcTransport T T where
  rpcTransport = identity

instance Loadable T where
  configFile = [relfile|websocket.yaml|]
