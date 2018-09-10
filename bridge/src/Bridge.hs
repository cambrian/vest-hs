module Bridge
  ( Config(..)
  , T(..)
  , make
  , serveRPC
  , callRPC
  , callRPCTimeout
  ) where

import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: Id
  , responseQueue :: Id
    -- Other metadata (time?).
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed.
  | EndOfResults
  | Error Text
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- other metadata
  , res :: Response
  } deriving (Eq, Show, Read)

data Config = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  } deriving (Eq, Show, Read)

data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Id
  , waitingCalls :: HashTable Id (Response -> IO ())
  }

data BridgeException
  = BadCall Text
  | InternalBridgeException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception BridgeException

-- Connects to bridge, begins listening on RPC queue.
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
         Just ResponseMessage {requestId, res} -> do
           waitingCall <- HashTable.lookup waitingCalls requestId
           case waitingCall of
             Just enqueue -> enqueue res
             Nothing ->
               throwIO
                 (InternalBridgeException
                    (Text.append "no handler for request id: " (show requestId)))
       AMQP.ackEnv env)
  return T {conn, chan, responseQueue = Id queueName, waitingCalls}

serveRPC ::
     (Read req, Show res) => T -> Route -> (req -> Streamly.Serial res) -> IO ()
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
           Just RequestMessage {id = requestId, responseQueue, req} -> do
             let Id _responseQueue = responseQueue
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
                     {requestId, res = Error (Text.append "bad input: " req)}
                 return ()
               Just r -> do
                 let results = handler r
                 Streamly.runStream $
                   Streamly.mapM
                     (\res ->
                        publish
                          ResponseMessage
                            {requestId, res = Result (Text.pack (show res))})
                     results
                 publish ResponseMessage {requestId, res = EndOfResults}
                 AMQP.ackEnv env
       return ())
  return ()

-- Timeouts are between successive updates.
callRPCTimeout ::
     forall req res. (Show req, Read res)
  => DiffTime
  -> T
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout maxTimeBetweenUpdates T {chan, responseQueue, waitingCalls} route req = do
  let Route queueName = route
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg = RequestMessage {id, responseQueue, req = Text.pack (show req)}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (push, results) <- repeatableTimeoutStream maxTimeBetweenUpdates
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

-- Has default timeout of 5 seconds.
callRPC :: (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC = callRPCTimeout (secondsToDiffTime 5)
