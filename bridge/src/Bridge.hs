module Bridge
  ( Config(..)
  , T(..)
  , Route(..)
  , make
  , serveRPC
  , callRPC
  , callRPCTimeout
  ) where

import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
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

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: Id
  , responseQueue :: Route
    -- other metadata (time?)
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed
  | EndOfResults
  | Error Text
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { id :: Id
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
  , responseQueue :: Route
  , waitingCalls :: HashTable Id (Response -> IO ())
  }

data BridgeException
  = Timeout Text
  | BadCall Text
  | InternalBridgeException Text -- if you get one of these, file a bug report
  deriving (Eq, Show, Read, Typeable, Ord)

instance Exception BridgeException

-- Connects to bridge, begins listening on RPC queue
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  myHostname <- Network.HostName.getHostName
  uuid <- UUID.nextRandom
  let queueName = Text.pack (myHostname ++ (UUID.toString uuid))
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  waitingCalls <- HashTable.new
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       let rawMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
       case readMaybe rawMsg of
         Nothing -> return ()
         Just ResponseMessage {id, res} -> do
           waitingCall <- HashTable.lookup waitingCalls id
           case waitingCall of
             Just enqueue -> enqueue res
             Nothing ->
               throwIO
                 (InternalBridgeException
                    (Text.append "no waiting call with id: " (show id)))
       AMQP.ackEnv env)
  return T {conn, chan, responseQueue = Route queueName, waitingCalls}

serveRPC :: (Read req, Show res) => T -> Route -> Handler req res -> IO ()
serveRPC T {chan} route handler = do
  let Route queueName = route
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       forkIO $ do
         let rawReqMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
         case readMaybe rawReqMsg of
           Nothing -> return ()
           Just RequestMessage {id, responseQueue, req} -> do
             let Route _responseQueue = responseQueue
             let publish resMsg =
                   let msgBody = ByteString.Lazy.UTF8.fromString $ show resMsg
                    in AMQP.publishMsg
                         chan
                         "" -- Default exchange just sends message to queue specified by routing key.
                         _responseQueue -- Exchange routing key.
                         AMQP.newMsg {AMQP.msgBody}
             case readMaybe (Text.unpack req) of
               Nothing -> do
                 publish
                   ResponseMessage
                     {id, res = Error (Text.append "bad input: " req)}
                 return ()
               Just r -> do
                 let results = handler r
                 Streamly.runStream $
                   Streamly.mapM
                     (\res ->
                        publish
                          ResponseMessage
                            {id, res = Result (Text.pack (show res))})
                     results
                 publish ResponseMessage {id, res = EndOfResults}
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
  let Route queueName = route
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg = RequestMessage {id, responseQueue, req = Text.pack (show req)}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (push, results) <- repeatablePushStream maxTimeBetweenUpdates
  HashTable.insert
    waitingCalls
    id
    (\response -> do
       maybeResult <-
         case response of
           Error msg -> throwIO (BadCall msg)
           Result res -> return $ Just (read @res (Text.unpack res))
           EndOfResults -> do
             HashTable.delete waitingCalls id
             return Nothing
       push maybeResult)
  _ <- AMQP.publishMsg chan "" queueName AMQP.newMsg {AMQP.msgBody}
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
