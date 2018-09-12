module SocketServer
  ( T(..)
  , SocketClientConfig(..) -- Purely for testing purposes.
  , makeServer
  , killServer
  , serveRPC
  , serveRPC'
  , makeClientConfig -- Purely for testing purposes.
  , callRPC -- Purely for testing purposes.
  , callRPC' -- Purely for testing purposes.
  , callRPCTimeout -- Purely for testing purposes.
  , callRPCTimeout' -- Purely for testing purposes.
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as Http
import qualified Network.Socket.Internal as Socket
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
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read, Generic)

instance FromJSON RequestMessage

instance ToJSON RequestMessage

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed.
  | EndOfResults
  | Error Text
  deriving (Eq, Show, Read, Generic)

instance FromJSON Response

instance ToJSON Response

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- Other metadata.
  , res :: Response
  } deriving (Eq, Show, Read, Generic)

instance FromJSON ResponseMessage

instance ToJSON ResponseMessage

data T = T
  { threadId :: ThreadId
  , clients :: HashTable Id WS.Connection
  , directHandlers :: HashTable Route (Text -> IO Response)
  , streamingHandlers :: HashTable Route (Text -> IO (Streamly.Serial Response))
  }

data SocketClientConfig = SocketClientConfig
  { uri :: URI
  , port :: Port
  , path :: Route
  }

data SocketServerException
  = BadCall Text
  | HandlerAlreadyExists Text
  | InternalSocketServerException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception SocketServerException

-- Creates socket server and starts listening.
makeServer :: Int -> IO T
makeServer port = do
  clients <- HashTable.new
  directHandlers <- HashTable.new
  streamingHandlers <- HashTable.new
  socketServerVar <- MVar.newEmptyMVar
  -- Warp runs forever on its own thread.
  threadId <-
    forkIO $ do
      socketServer <- MVar.readMVar socketServerVar
      -- Wait for thread ID to be added to server state.
      Warp.run port $
        WS.websocketsOr WS.defaultConnectionOptions (wsApp socketServer) httpApp
  let socketServer = T {threadId, clients, directHandlers, streamingHandlers}
  MVar.putMVar socketServerVar socketServer
  -- Wait for init to avoid race conditions.
  threadDelay 100000 -- TODO: Make less jank.
  return socketServer

killServer :: T -> IO ()
killServer T {threadId} = do
  killThread threadId

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "not a WebSocket request"

wsApp :: T -> WS.ServerApp
wsApp socketServer pendingConn = do
  conn <- WS.acceptRequest pendingConn
  -- Each connection is on a new thread.
  clientId <- connectClient socketServer conn
  WS.forkPingThread conn 30 -- TODO: Make configurable.
  Exception.finally
    (serveClient socketServer conn)
    (disconnectClient socketServer clientId)

connectClient :: T -> WS.Connection -> IO Id
connectClient socketServer conn = do
  let T {clients} = socketServer
  clientIdRaw <- UUID.nextRandom
  let clientId = UUID.toText clientIdRaw
  HashTable.insert clients (Id clientId) conn
  return $ Id clientId

disconnectClient :: T -> Id -> IO ()
disconnectClient T {clients} clientId = do
  HashTable.delete clients clientId

serveClient :: T -> WS.Connection -> IO ()
serveClient T {directHandlers, streamingHandlers} conn =
  Monad.forever $ do
    rawReqMsg <- WS.receiveData conn
    case decode rawReqMsg of
      Nothing -> return ()
      Just reqMsg -> do
        let RequestMessage {id, route, req} = reqMsg
        let (Route _route) = route
        let publish result = do
              let resMsg = ResponseMessage {requestId = id, res = result}
              WS.sendTextData conn (encode resMsg)
        directMaybe <- HashTable.lookup directHandlers route
        -- Look for a direct handler first and service request.
        case directMaybe of
          Nothing -> do
            streamingMaybe <- HashTable.lookup streamingHandlers route
            -- Look for a streaming handler otherwise.
            case streamingMaybe of
              Nothing -> do
                publish . Error . (Text.append "invalid route: ") $ _route
              Just handler -> do
                results <- handler req
                Streamly.runStream $ Streamly.mapM publish results
                publish EndOfResults
          Just handler -> do
            result <- handler req
            publish result

parseSReqMaybe :: (FromJSON req) => Text -> Maybe req
parseSReqMaybe = decode . ByteString.Lazy.UTF8.fromString . Text.unpack

badInput :: Text -> Response
badInput = Error . (Text.append "bad input: ")

toSerialText :: (ToJSON r) => r -> Text
toSerialText = Text.pack . ByteString.Lazy.UTF8.toString . encode

resToResponse :: (ToJSON res) => res -> Response
resToResponse = Result . toSerialText

serveRPC :: (FromJSON req, ToJSON res) => T -> Route -> (req -> IO res) -> IO ()
serveRPC T {directHandlers, streamingHandlers} route handler = do
  let Route _route = route
  let serialTextHandler sReq =
        case parseSReqMaybe sReq of
          Nothing -> return $ badInput sReq
          Just req -> do
            res <- handler req
            return $ resToResponse res
  -- Error if a streaming handler has the same route name.
  streamingHandler <- HashTable.lookup streamingHandlers route
  case streamingHandler of
    Nothing -> HashTable.insert directHandlers route serialTextHandler
    Just _ -> throwIO $ HandlerAlreadyExists _route

serveRPC' ::
     (FromJSON req, ToJSON res)
  => T
  -> Route
  -> (req -> IO (Streamly.Serial res))
  -> IO ()
serveRPC' T {directHandlers, streamingHandlers} route handler = do
  let Route _route = route
  let serialTextHandler sReq = do
        case parseSReqMaybe sReq of
          Nothing -> return $ Streamly.yield . badInput $ sReq
          Just req -> do
            resStream <- handler req
            return $ Streamly.map resToResponse resStream
  -- Error if a direct handler has the same route name.
  directHandler <- HashTable.lookup directHandlers route
  case directHandler of
    Nothing -> HashTable.insert streamingHandlers route serialTextHandler
    Just _ -> throwIO $ HandlerAlreadyExists _route

-- Creates config for a socket client.
makeClientConfig :: Text -> Int -> Text -> SocketClientConfig
makeClientConfig _uri _port _path =
  let uri = URI _uri
   in let port = Port _port
       in let path = Route _path
           in SocketClientConfig {uri, port, path}

_callRPCTimeout ::
     forall req res. (ToJSON req)
  => IO (Response -> IO (), IO res, IO ())
  -> DiffTime
  -> SocketClientConfig
  -> Route
  -> req
  -> IO res
_callRPCTimeout helpers _timeout SocketClientConfig {uri, port, path} route req = do
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let (URI _uri, Port _port, Route _path) = (uri, port, path)
  let (uriStr, pathStr) = (Text.unpack _uri, Text.unpack _path)
  let reqMsg = RequestMessage {id, route, req = toSerialText req}
  let rawReqMsg = encode reqMsg
  (push, result, waitForDone) <- helpers
  renewTimeout <- timeoutThrowIO' waitForDone _timeout
  -- Fork a thread that writes incoming WS data to the stream.
  -- Then send the outbound request and wait for reader thread to finish.
  let wsClientApp conn =
        catch
          (do readerThreadId <-
                forkIO $ do
                  let readLoop = do
                        rawResMsg <- WS.receiveData conn
                        renewTimeout
                        case decode rawResMsg of
                          Nothing -> return () -- TODO: Throw here?
                          Just (ResponseMessage {res}) -> push res
                        readLoop
                  readLoop
              WS.sendTextData conn rawReqMsg
              waitForDone >> killThread readerThreadId
              WS.sendClose conn ("[EOF]" :: Text))
          (\e -> do
             let error = Text.pack $ show (e :: WS.ConnectionException)
             -- ConnectionClosed is expected if the thread was forcefully shut down in the middle
             -- of waiting for WS.receiveData (at least that's how it appears).
             when (error /= "ConnectionClosed") (throwIO e))
  Socket.withSocketsDo $ WS.runClient uriStr _port pathStr wsClientApp
  result

callRPCTimeout ::
     forall req res. (ToJSON req, FromJSON res)
  => DiffTime
  -> SocketClientConfig
  -> Route
  -> req
  -> IO res
callRPCTimeout =
  _callRPCTimeout
    (do resultVar <- MVar.newEmptyMVar
        let push =
              \case
                Error errorMsg -> throwIO $ BadCall errorMsg
                Result resText -> do
                  let lazyRes =
                        resText & Text.unpack & ByteString.Lazy.UTF8.fromString
                  case decode @res lazyRes of
                    Nothing -> return () -- TODO: Handle failure here?
                    Just res -> MVar.putMVar resultVar res
                EndOfResults ->
                  throwIO $
                  InternalSocketServerException
                    "direct RPC should never get EndOfResults"
        let result = MVar.readMVar resultVar
        return (push, result, void result))

callRPCTimeout' ::
     forall req res. (ToJSON req, FromJSON res)
  => DiffTime
  -> SocketClientConfig
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout' =
  _callRPCTimeout
    (do (push, results) <- repeatableStream
        let pushHelper res = do
              maybeResult <-
                case res of
                  Error errorMsg -> throwIO (BadCall errorMsg)
                  Result resText -> do
                    let lazyRes =
                          resText & Text.unpack &
                          ByteString.Lazy.UTF8.fromString
                    return $ decode @res lazyRes -- TODO: Handle failure here?
                  EndOfResults -> return Nothing
              push maybeResult
        let waitForDone = Streamly.mapM_ return results
        return (pushHelper, return results, waitForDone))

-- Has default timeout of 5 seconds.
callRPC ::
     (ToJSON req, FromJSON res) => SocketClientConfig -> Route -> req -> IO res
callRPC = callRPCTimeout (secondsToDiffTime 5)

-- Has default timeout of 5 seconds.
callRPC' ::
     (ToJSON req, FromJSON res)
  => SocketClientConfig
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPC' = callRPCTimeout' (secondsToDiffTime 5)
