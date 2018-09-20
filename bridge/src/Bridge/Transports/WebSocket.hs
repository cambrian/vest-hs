module Bridge.Transports.WebSocket
  ( T(..)
  , Config(..)
  , localConfig
  ) where

import Bridge.PubSub
import Bridge.Rpc
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
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
  { id :: Id
  , route :: Route
    -- Other metadata (time?).
  , reqText :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- Other metadata.
  , response :: Either RpcClientException Text
  } deriving (Eq, Show, Read)

data Config = Config
  { serverPort :: Port
  , pingInterval :: Int
  , clientUri :: URI
  , clientPort :: Port
  , clientPath :: Route
  }

localConfig :: Config
localConfig =
  Config
    { serverPort = Port 3000
    , pingInterval = 30
    , clientUri = URI "127.0.0.1"
    , clientPort = Port 3000
    , clientPath = Route "/"
    }

data T = T
  { serverThread :: Async ()
  , clients :: HashTable Id WS.Connection
  , servedRouteRequests :: HashTable Route ( (Id, RequestMessage) -> IO () -- Request pusher.
                                           , IO ()
                                           , Streamly.Serial ( Id
                                                             , RequestMessage))
  , servedConnectionResponses :: HashTable Id ( Text -> IO () -- Response pusher.
                                              , IO ()
                                              , Streamly.Serial Text)
  , clientUri :: URI
  , clientPort :: Port
  , clientPath :: Route
  }

instance Resource T where
  type ResourceConfig T = Config
  hold :: Config -> IO T
  -- Connects to bridge, begins listening on RPC queue.
  hold Config {serverPort, pingInterval, clientUri, clientPort, clientPath} = do
    clients <- HashTable.new
    servedRouteRequests <- HashTable.new
    servedConnectionResponses <- HashTable.new
    -- Warp runs forever on its own thread.
    serverThread <-
      async $ do
        socketServer <- readMVar socketServerVar
        -- Wait for thread ID to be added to server state.
        Warp.run port $
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
  release T {serverThread, servedRouteRequests} = do
    HashTable.mapM_ (\(route, ((_, close, _))) -> close) servedRouteRequests
    HashTable.mapM_
      (\(route, ((_, close, _))) -> close)
      servedConnectionResponses
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

connectClient :: T -> WS.Connection -> IO Id
connectClient socketServer conn = do
  let T {clients} = socketServer
  clientIdRaw <- newUuid
  HashTable.insert clients (Id clientId) conn
  return $ Id clientId

disconnectClient :: T -> Id -> IO ()
disconnectClient T {clients, servedConnectionResponses} clientId = do
  HashTable.delete clients clientId
  HashTable.delete servedConnectionResponses clientId

serveClient :: T -> Id -> WS.Connection -> IO ()
serveClient T {servedRouteRequests} clientId conn = do
  responses <- nonRepeatablePushStream
  HashTable.insert servedConnectionResponses clientId responses
  let (_, _, streamOut) = responses
  -- Thread dies on its own when the response is complete.
  async $ Streamly.mapM_ (WS.sendTextData conn) streamOut
  Monad.forever $ do
    msg <- WS.receiveData conn
    decode msg >|>|
      (\reqMsg -> do
         let RequestMessage {route, reqText} = reqMsg
         requestsMaybe <- HashTable.lookup servedRouteRequests route
         requestsMaybe >|>| (\(pushIn, _, _) -> pushIn (clientId, reqMsg)))

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
       -- (publish -> res/Stream res -> IO ())
       -- Generic publisher on intermediate result x. Should encapsulate serializing the
       -- intermediate results.
    -> (Text -> Maybe req)
    -> Route
       -- Should throw if Route is already being served.
    -> T
    -> (req -> IO x)
    -> IO ()
    -- Should mutate t to store the details necessary for cleanup.
  _serve publisher deserialize (Route _route) T { servedRouteRequests
                                                , servedConnectionResponses
                                                } handler = do
    (_, _, streamIn) <-
      HashTable.lookup servedRouteRequests (Route _route) >>= \case
        Nothing -> do
          requests <- nonRepeatablePushStream
          HashTable.insert servedRouteRequests (Route _route) requests
          return requests
        Just _ -> throw $ AlreadyServing (Route _route)
    let handleMsg (clientId, RequestMessage {id = requestId, route, reqText}) = do
          let pub response = do
                outMaybe <- HashTable.lookup servedConnectionResponses clientId
                case outMaybe of
                  Nothing -> return () -- Rare but possible edge case.
                  Just (pushOut, _, _) ->
                    void . pushOut . encode $
                    ResponseMessage {requestId, response}
          case deserialize reqText of
            Nothing -> pub . Left . BadCall $ "bad input: " <> reqText
            Just r -> handler r >>= publisher (pub . Right)
    -- Thread dies on its own when stream closed on kill.
    async $ Streamly.mapM_ handleMsg streamIn

_callRPCTimeout ::
     forall req res. (ToJSON req)
  => IO (Response -> IO (), IO res, IO ())
  -> Time Second
  -> SocketClientConfig
  -> Route
  -> req
  -> IO res
_callRPCTimeout helpers _timeout SocketClientConfig {uri, port, path} route req = do
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let (URI _uri, Port _port, Route _path) = (uri, port, path)
  let (uriStr, pathStr) = (Text.unpack _uri, Text.unpack _path)
  let reqMsg = RequestMessage {id, route, req = encodeSerialText req}
  let rawReqMsg = encode reqMsg
  (push, result, waitForDone) <- helpers
  renewTimeout <- timeoutThrow' waitForDone _timeout
  -- Fork a thread that writes incoming WS data to the stream.
  -- Then send the outbound request and wait for reader thread to finish.
  let wsClientApp conn = do
        readerThread <-
          async $ do
            let readLoop = do
                  rawResMsg <- WS.receiveData conn
                  renewTimeout
                  case decode rawResMsg of
                    Nothing -> return () -- Swallow if entire message is garbled.
                    Just (ResponseMessage {res}) ->
                      case res of
                        Left ex -> throw ex -- TODO: Throw async?
                        Right resp -> push resp
                  readLoop
            readLoop
        WS.sendTextData conn rawReqMsg
        waitForDone >> cancel readerThread
  Socket.withSocketsDo $ WS.runClient uriStr _port pathStr wsClientApp
  result

callRPCTimeout ::
     forall req res. (ToJSON req, FromJSON res)
  => Time Second
  -> SocketClientConfig
  -> Route
  -> req
  -> IO res
callRPCTimeout =
  _callRPCTimeout
    (do resultVar <- newEmptyMVar
        let push =
              \case
                Result resText -> do
                  case decodeSerialText resText of
                    Nothing -> throw $ DecodeException resText
                    Just res -> putMVar resultVar res
                EndOfResults ->
                  throw $
                  InternalSocketServerException
                    "direct RPC should never get EndOfResults"
        let result = readMVar resultVar
        return (push, result, void result))

callRPCTimeout' ::
     forall req res. (ToJSON req, FromJSON res)
  => Time Second
  -> SocketClientConfig
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout' =
  _callRPCTimeout
    (do (_push, close, results) <- repeatableStream
        -- Decoding returns Maybe (Maybe result) where the outer Maybe indicates whether the result
        -- is to be pushed or not, and the inner Maybe indicates stream continuation.
        let push = do
              \case
                Result resText -> do
                  case decodeSerialText resText of
                    Nothing -> throw $ DecodeException resText
                    Just resActual -> _push resActual
                EndOfResults -> close
        let waitForDone = Streamly.mapM_ return results
        return (push, return results, waitForDone))

defaultTimeout :: Time Second
defaultTimeout = sec 1

-- Direct RPC call with default timeout.
callRPC ::
     (ToJSON req, FromJSON res) => SocketClientConfig -> Route -> req -> IO res
callRPC = callRPCTimeout defaultTimeout

-- Streaming RPC call with default timeout.
callRPC' ::
     (ToJSON req, FromJSON res)
  => SocketClientConfig
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPC' = callRPCTimeout' defaultTimeout
