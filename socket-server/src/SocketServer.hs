module SocketServer
  ( T(..)
  , makeServer
  , makeClient
  , serveRPC
  , callRPC
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe
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

data SocketServerException
  = BadCall Text
  | InternalSocketServerException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception SocketServerException

-- Creates socket server and starts listening.
makeServer :: Port -> IO T
makeServer port = do
  clients <- HashTable.new
  handlers <- HashTable.new
  let state = T {clients, handlers}
  forkIO $
    -- Warp runs forever as an IO action, so put it on its own thread.
   do
    Warp.run 3000 $
      WS.websocketsOr WS.defaultConnectionOptions (wsApp state) httpApp
  return state

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "not a WebSocket request"

wsApp :: T -> WS.ServerApp
wsApp state pendingConn = do
  let T {clients} = state
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient state conn
  -- Each connection gets its own handler thread.
  WS.forkPingThread conn 30 -- TODO: Make configurable.
  Exception.finally (serveClient state conn) (disconnectClient state clientId)

connectClient :: T -> WS.Connection -> IO Id
connectClient state conn = do
  let T {clients} = state
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
        let RequestMessage {id = _id, route = _route, req} = reqMsg
        let (Id id) = _id
        let (Route route) = _route
        let publish result = do
              let resMsg = ResponseMessage {requestId = _id, res = result}
              WS.sendTextData conn (encode resMsg)
        handlerMaybe <- HashTable.lookup handlers _route
        case handlerMaybe of
          Nothing -> do
            publish (Error (Text.append "invalid route: " route))
          Just handler -> do
            let results = handler req
            Streamly.runStream $ Streamly.mapM publish results
            publish EndOfResults
    return ()

serveRPC ::
     (Read req, Show res, FromJSON req, ToJSON res)
  => T
  -> Route
  -> (req -> Streamly.Serial res)
  -> IO ()
serveRPC T {handlers} route handler = do
  let serialTextHandler sReq =
        let reqMaybe =
              sReq & Text.unpack & ByteString.Lazy.UTF8.fromString & decode
         in case reqMaybe of
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

-- Creates socket server and starts listening.
makeClient :: Text -> Port -> Text -> IO (WS.ClientApp ())
makeClient uri port route = do
  clients <- HashTable.new
  handlers <- HashTable.new
  let state = T {clients, handlers}
  forkIO $
    -- Warp runs forever as an IO action, so put it on its own thread.
   do
    Warp.run 3000 $
      WS.websocketsOr WS.defaultConnectionOptions (wsApp state) httpApp
  return state

callRPCTimeout ::
     forall req res. (Show req, Read res)
  => ClientConfig
  -> DiffTime
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout maxTimeBetweenUpdates _route req = do
  let Route route = _route
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg = RequestMessage {id, route, req = Text.pack (show req)}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (push, results) <- repeatableTimeoutStream maxTimeBetweenUpdates
  let wsClientApp conn = do
        forkIO $ -- Fork a thread that writes WS data to stream. Read until result is the end or an
                 -- error occurs and forces us to stop reading.
         do
          let readLoop = do
                msg <- WS.receiveData conn
                let resMsgMaybe = decode msg
                shouldContinue <-
                  case resMsgMaybe of
                    Nothing -> return () -- TODO: Log error?
                    Just (ResponseMessage {requestId, res}) -> do
                      let resMaybe = decode res
                      case resMaybe of
                        Nothing -> return () -- TODO: Log error?
                        Just resOuter -> do
                          maybeResult <-
                            case resOuter of
                              Error msg -> throwIO (BadCall msg)
                              Result res ->
                                return $ decode @res (Text.unpack res)
                              EndOfResults -> return Nothing
                          push maybeResult
                          return (maybeResult /= Nothing)
                when (shouldContinue) readLoop
          readLoop
          -- Read from stdin and write to WS
        WS.sendClose conn ()
  withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app

-- Has default timeout of 5 seconds.
callRPC :: (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC = callRPCTimeout (secondsToDiffTime 5)
