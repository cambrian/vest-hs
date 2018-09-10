module SocketServer
  ( T(..)
  -- , make
  -- , serveRPC
  -- , testRPC
  , module Protolude
  ) where

-- import Control.Concurrent (forkIO)
-- import qualified Control.Concurrent.STM.TVar as TVar
-- import Control.Monad (unless)
-- import qualified Control.Monad.STM as STM
-- import qualified Data.ByteString.Lazy.UTF8
-- import Data.Function ((&))
-- import Data.Text (Text, append, pack, unpack)
-- import qualified Data.Vector as Vector
-- import qualified Network.AMQP as AMQP
-- import qualified Network.HostName
-- import Text.Read (read, readMaybe)
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception (Exception, throw)
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.HashTable.IO as HashTable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text, append, pack, unpack)
import Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Protolude
import qualified Safe
import qualified Streamly
import qualified Streamly.Prelude as Streamly

type Handler req res = req -> Streamly.Serial res

type HashTable k v = HashTable.BasicHashTable k v

type Id = Text

type Port = Int

type Route = Text

data SocketBridgeException =
  SocketBridgeException Text
  deriving (Eq, Show, Read, Typeable)

instance Exception SocketBridgeException

data RequestMessage = RequestMessage
  { requestId :: Id
    -- Other metadata (time?).
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed.
  | End_of_results
  | Error Text
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- Other metadata.
  , res :: Response
  } deriving (Eq, Show, Read)

data T = T
  { clients :: HashTable Id WS.Connection
  , handlers :: HashTable Text (RequestMessage -> Streamly.Serial ResponseMessage)
  }

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "not a websocket request"
-- -- Creates socket server and starts listening.
-- make :: Port -> IO T
-- make port = do
--   let clients = HashTable.new
--   let handlers = HashTable.new
--   let state = T {clients, handlers}
--   forkIO $
--     -- Warp runs forever as an IO action, so put it on its own thread.
--    do
--     Warp.run 3000 $
--       WS.websocketsOr WS.defaultConnectionOptions (wsApp state) httpApp
--   return state
-- connectClient :: T -> WS.Connection -> IO Id
-- connectClient T {clients} conn = do
--   clientIdRaw <- UUID.nextRandom
--   let clientId = UUID.toText clientIdRaw
--   HashTable.insert clients clientId conn
--   return clientId
-- disconnectClient :: T -> Id -> IO ()
-- disconnectClient T {clients} clientId = do
--   HashTable.delete clients clientId
-- serve :: T -> WS.Connection -> IO ()
-- serve T {handlers} conn =
--   Monad.forever $ do
--     rawReqMsg <- WS.receiveData conn
--     case readMaybe rawReqMsg of
--       Nothing -> return ()
--       Just reqMsg -> do
--         let RequestMessage {requestId, req} = reqMsg
--         case readMaybe (unpack req) of
--           Nothing -> do
--             publish
--               (show
--                  ResponseMessage
--                    {requestId, res = Error (Data.Text.append "bad input: " req)})
--             return ()
--           Just r -> do
--             let results = handler r
--             Streamly.runStream $
--               Streamly.mapM
--                 (\res ->
--                    publish
--                      (show
--                         ResponseMessage
--                           {requestId, res = Result (pack (show res))}))
--                 results
--     return ()
-- serveRPC :: (Read req, Show res) => T -> Route -> Handler req res -> () -- IO ()?
-- serveRPC :: (Read req, Show res) => T -> Route -> Handler req res -> IO ()
-- serveRPC T {chan} route handler = do
--   AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName = route}
--   AMQP.consumeMsgs
--     chan
--     route
--     AMQP.Ack
--     (\(msg, env) -> do
--        forkIO $ do
--          let rawReqMsg = msg & AMQP.msgBody & Data.ByteString.Lazy.UTF8.toString
--          case readMaybe rawReqMsg of
--            Nothing -> return ()
--            Just reqMsg -> do
--              let RequestMessage {requestId, responseQueue, req} = reqMsg
--              -- TODO: figure out how to restrict a to Show instances
--              let publish a =
--                    let msgBody = Data.ByteString.Lazy.UTF8.fromString a
--                     in AMQP.publishMsg
--                          chan
--                          "" -- Default exchange just sends message to queue specified by routing key.
--                          responseQueue -- Exchange routing key.
--                          AMQP.newMsg {AMQP.msgBody}
--              case readMaybe (unpack req) of
--                Nothing -> do
--                  publish
--                    (show
--                       ResponseMessage
--                         { requestId
--                         , res = Error (Data.Text.append "bad input: " req)
--                         })
--                  return ()
--                Just r -> do
--                  let results = handler r
--                  Streamly.runStream $
--                    Streamly.mapM
--                      (\res ->
--                         publish
--                           (show
--                              ResponseMessage
--                                {requestId, res = Result (pack (show res))}))
--                      results
--                  publish
--                    (show ResponseMessage {requestId, res = End_of_results})
--                  AMQP.ackEnv env
--        return ())
--   return ()
-- callRPC ::
--      forall req res. (Show req, Read res)
--   => T
--   -> Route
--   -> req
--   -> IO (Streamly.Serial res)
-- callRPC T {chan, responseQueue, waitingCalls} route req = do
--   requestIdRaw <- UUID.nextRandom
--   let requestId = UUID.toText requestIdRaw
--   let reqMsg = RequestMessage {requestId, responseQueue, req = pack (show req)}
--   let msgBody = Data.ByteString.Lazy.UTF8.fromString . show $ reqMsg
--   resultsVar <- TVar.newTVarIO Vector.empty
--   HashTable.insert
--     waitingCalls
--     requestId
--     (\response -> do
--        maybeResult <-
--          case response of
--            Error msg -> throw (BridgeException msg)
--            Result res -> return $ Just (read @res (unpack res))
--            End_of_results -> do
--              HashTable.delete waitingCalls requestId
--              return Nothing
--        STM.atomically $ TVar.modifyTVar resultsVar (`Vector.snoc` maybeResult))
--   AMQP.publishMsg chan "" route AMQP.newMsg {AMQP.msgBody}
--   return $
--     Streamly.unfoldrM
--       (\idx ->
--          STM.atomically $ do
--            results <- TVar.readTVar resultsVar
--            unless (idx < Vector.length results) STM.retry
--            case Vector.unsafeIndex results idx of
--              Nothing -> return Nothing
--              Just x -> return (Just (x, idx + 1)))
--       0
