module SocketServer
  ( T(..)
  , makeServer
  , makeClient -- Purely for testing purposes.
  , serveRPC
  , callRPC -- Purely for testing purposes.
  , callRPCTimeout -- Purely for testing purposes.
  ) where

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
  { clients :: HashTable Id WS.Connection
  , handlers :: HashTable Route (Text -> Streamly.Serial Response) -- Serial req to response.
  }

data SocketClient = SocketClient
  { uri :: URI
  , port :: Port
  , path :: Route
  }

data SocketServerException
  = BadCall Text
  | InternalSocketServerException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception SocketServerException

-- Creates socket server and starts listening.
makeServer :: Int -> IO T
makeServer port = do
  clients <- HashTable.new
  handlers <- HashTable.new
  let socketServer = T {clients, handlers}
  forkIO $
    -- Warp runs forever as an IO action, so put it on its own thread.
   do
    Warp.run port $
      WS.websocketsOr WS.defaultConnectionOptions (wsApp socketServer) httpApp
  return socketServer

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "not a WebSocket request"

wsApp :: T -> WS.ServerApp
wsApp socketServer pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient socketServer conn
  -- Each connection gets its own handler thread.
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
  return (Id clientId)

disconnectClient :: T -> Id -> IO ()
disconnectClient T {clients} clientId = do
  HashTable.delete clients clientId

serveClient :: T -> WS.Connection -> IO ()
serveClient T {handlers} conn =
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
        handlerMaybe <- HashTable.lookup handlers route
        case handlerMaybe of
          Nothing -> do
            publish (Error (Text.append "invalid route: " _route))
          Just handler -> do
            let results = handler req
            Streamly.runStream $ Streamly.mapM publish results
            publish EndOfResults
    return ()

serveRPC ::
     (FromJSON req, ToJSON res)
  => T
  -> Route
  -> (req -> Streamly.Serial res)
  -> IO ()
serveRPC T {handlers} route handler = do
  let serialTextHandler sReq =
        let reqLazyStr = sReq & Text.unpack & ByteString.Lazy.UTF8.fromString
         in case decode reqLazyStr of
              Nothing -> Streamly.yield (Error (Text.append "bad input: " sReq))
              Just req ->
                Streamly.map
                  (\res ->
                     let sRes =
                           res & encode & ByteString.Lazy.UTF8.toString &
                           Text.pack
                      in (Result sRes))
                  (handler req)
  HashTable.insert handlers route serialTextHandler

-- Creates config for a socket client.
makeClient :: Text -> Int -> Text -> SocketClient
makeClient _uri _port _path =
  let uri = URI _uri
   in let port = Port _port
       in let path = Route _path
           in SocketClient {uri, port, path}

callRPCTimeout ::
     forall req res. (ToJSON req, FromJSON res)
  => DiffTime
  -> SocketClient
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout maxTimeBetweenUpdates SocketClient {uri, port, path} route req = do
  let URI _uri = uri
  let Port _port = port
  let Route _path = path
  let uriStr = Text.unpack _uri
  let pathStr = Text.unpack _path
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg =
        RequestMessage
          { id
          , route
          , req = req & encode & ByteString.Lazy.UTF8.toString & Text.pack
          }
  let rawReqMsg = encode reqMsg
  (push, results) <- repeatableTimeoutStream maxTimeBetweenUpdates
  let wsClientApp conn = do
        forkIO $ -- Fork a thread that writes WS data to stream. Read until result is the end or an
                 -- error occurs and forces us to stop reading.
         do
          let readLoop = do
                rawResMsg <- WS.receiveData conn
                result <-
                  case decode rawResMsg of
                    Nothing -> return Nothing -- TODO: Log error?
                    Just (ResponseMessage {res}) -> do
                      maybeResult <-
                        case res of
                          Error errorMsg -> throwIO (BadCall errorMsg)
                          Result resText -> do
                            let lazyRes =
                                  resText & Text.unpack &
                                  ByteString.Lazy.UTF8.fromString
                            return $ decode @res lazyRes
                          EndOfResults -> return Nothing
                      push maybeResult
                      return maybeResult
                when (isJust result) readLoop
          readLoop
        WS.sendTextData conn rawReqMsg
        WS.sendClose conn ("[EOF]" :: Text)
  Socket.withSocketsDo $ WS.runClient uriStr _port pathStr wsClientApp
  return results

-- Has default timeout of 5 seconds.
callRPC ::
     (ToJSON req, FromJSON res)
  => SocketClient
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPC = callRPCTimeout (secondsToDiffTime 5)
