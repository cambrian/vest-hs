module Bridge
  ( Config(..)
  , T(..)
  , make
  , serveRPC
  , callRPC
  , callRPCTimeout
  ) where

import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import Data.Text (Text, append, pack, unpack)
import qualified Data.Time.Clock as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import System.Timeout (timeout)
import Text.Read (read, readMaybe)
import VestPrelude

type Id = Text

type Route = Text

type QueueName = Text

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { requestId :: Id
  , responseQueue :: QueueName
    -- other metadata (time?)
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed
  | EndOfResults
  | Error Text
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- other metadata
  , res :: Response
  } deriving (Eq, Show, Read)

type Handler req res = req -> Streamly.Serial res

data Config = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  } deriving (Eq, Show, Read)

data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: QueueName
  , waitingCalls :: HashTable Id (Response -> IO ())
  }

data BridgeException
  = Timeout Text
  | BadCall Text
  | InternalBridgeException Text -- if you get one of these, file a bug report
  deriving (Eq, Show, Read, Typeable)

instance Exception BridgeException

-- Connects to bridge, begins listening on RPC queue
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <- AMQP.openConnection (unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  myHostname <- Network.HostName.getHostName
  uuid <- UUID.nextRandom
  let responseQueue = pack (myHostname ++ (UUID.toString uuid))
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName = responseQueue}
  waitingCalls <- HashTable.new
  AMQP.consumeMsgs
    chan
    responseQueue
    AMQP.Ack
    (\(msg, env) -> do
       let rawMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
       case readMaybe rawMsg of
         Nothing -> return ()
         Just ResponseMessage {requestId, res} -> do
           waitingCall <- HashTable.lookup waitingCalls requestId
           case waitingCall of
             Just enqueue -> enqueue res
             Nothing ->
               throwIO
                 (InternalBridgeException
                    (append "no waiting call with requestId: " requestId))
       AMQP.ackEnv env)
  return T {conn, chan, responseQueue, waitingCalls}

serveRPC :: (Read req, Show res) => T -> Route -> Handler req res -> IO ()
serveRPC T {chan} route handler = do
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName = route}
  AMQP.consumeMsgs
    chan
    route
    AMQP.Ack
    (\(msg, env) -> do
       forkIO $ do
         let rawReqMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
         case readMaybe rawReqMsg of
           Nothing -> return ()
           Just RequestMessage {requestId, responseQueue, req} -> do
             let publish resMsg =
                   let msgBody = ByteString.Lazy.UTF8.fromString $ show resMsg
                    in AMQP.publishMsg
                         chan
                         "" -- Default exchange just sends message to queue specified by routing key.
                         responseQueue -- Exchange routing key.
                         AMQP.newMsg {AMQP.msgBody}
             case readMaybe (unpack req) of
               Nothing -> do
                 publish
                   ResponseMessage
                     { requestId
                     , res = Error (Data.Text.append "bad input: " req)
                     }
                 return ()
               Just r -> do
                 let results = handler r
                 Streamly.runStream $
                   Streamly.mapM
                     (\res ->
                        publish
                          ResponseMessage
                            {requestId, res = Result (pack (show res))})
                     results
                 publish ResponseMessage {requestId, res = EndOfResults}
                 AMQP.ackEnv env
       return ())
  return ()

-- Timeouts are between successive updates
callRPCTimeout ::
     forall req res. (Show req, Read res)
  => Time.DiffTime
  -> T
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout maxTimeBetweenUpdates T {chan, responseQueue, waitingCalls} route req = do
  requestId <- fmap UUID.toText UUID.nextRandom
  let reqMsg = RequestMessage {requestId, responseQueue, req = pack (show req)}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (push, results) <- repeatablePushStream maxTimeBetweenUpdates
  HashTable.insert
    waitingCalls
    requestId
    (\response -> do
       maybeResult <-
         case response of
           Error msg -> throwIO (BadCall msg)
           Result res -> return $ Just (read @res (unpack res))
           EndOfResults -> do
             HashTable.delete waitingCalls requestId
             return Nothing
       push maybeResult)
  _ <- AMQP.publishMsg chan "" route AMQP.newMsg {AMQP.msgBody}
  return results

repeatablePushStream ::
     Time.DiffTime -> IO (Maybe a -> IO (), Streamly.Serial a)
repeatablePushStream _timeout = do
  let timeoutMicros =
        Time.diffTimeToPicoseconds _timeout & fromIntegral & (* 1000000)
  resultsVar <- TVar.newTVarIO Vector.empty
  let push a = STM.atomically $ TVar.modifyTVar resultsVar (`Vector.snoc` a)
  let stream =
        Streamly.unfoldrM
          (\idx ->
             timeout
               timeoutMicros
               (STM.atomically $ do
                  results <- TVar.readTVar resultsVar
                  unless (idx < Vector.length results) STM.retry
                  case Vector.unsafeIndex results idx of
                    Nothing -> return Nothing
                    Just x -> return $ Just (x, idx + 1)) >>= \case
               Just result -> return result
               Nothing -> throwIO $ Timeout "timed out")
          0
  return (push, stream)

-- Has default timeout of 5 seconds
callRPC :: (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC = callRPCTimeout (Time.secondsToDiffTime 5)
