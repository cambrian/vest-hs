module Bridge
  ( Config(..)
  , T(..)
  , defaultTimeout
  , make
  , kill
  , serveRPC
  , serveRPC'
  , callRPC
  , callRPC'
  , callRPCTimeout
  , callRPCTimeout'
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.HashTable.ST.Basic as HashTableST
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

defaultTimeout :: DiffTime
defaultTimeout = secondsToDiffTime 10

data BridgeException
  = BadCall Text
  | InternalBridgeException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception BridgeException

data RequestMessage = RequestMessage
  { id :: Id
  , responseQueue :: Id
    -- Other metadata (time?).
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed.
  | EndOfResults
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- Other metadata.
  , res :: Either BridgeException Response
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
  , consumerTag :: AMQP.ConsumerTag
  , waitingCalls :: HashTable Id (Response -> IO ())
  }

amqpMsgToString :: AMQP.Message -> String.String
amqpMsgToString = ByteString.Lazy.UTF8.toString . AMQP.msgBody

generateNewQueueName :: IO Text
generateNewQueueName = do
  uuid <- UUID.nextRandom
  myHostName <- Network.HostName.getHostName
  return $ Text.pack (myHostName ++ "." ++ UUID.toString uuid)

-- Connects to bridge, begins listening on RPC queue.
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  queueName <- generateNewQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  waitingCalls <- HashTable.new
  consumerTag <-
    AMQP.consumeMsgs
      chan
      queueName
      AMQP.Ack
      (\(msg, env) -> do
         let rawMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
         case readMaybe rawMsg of
           Nothing -> return ()
           Just ResponseMessage {requestId, res} ->
             case res of
               Left exception -> throwIO exception
               Right r ->
                 HashTable.lookup waitingCalls requestId >>= mapM_ ($ r)
         AMQP.ackEnv env)
  return T {conn, chan, responseQueue = Id queueName, consumerTag, waitingCalls}

-- Kills and cleans up bridge.
kill :: T -> IO ()
kill T {conn, chan, responseQueue, consumerTag} = do
  let Id _responseQueue = responseQueue
  AMQP.cancelConsumer chan consumerTag
  _ <- AMQP.deleteQueue chan _responseQueue
  -- Closing connection implicitly closes chan.
  AMQP.closeConnection conn

-- RPC server.
_serveRPC ::
     (Read req)
  => ((Response -> IO ()) -> res -> IO ())
  -> T
  -> Route
  -> (req -> IO res)
  -> IO ()
_serveRPC publisher T {chan} route handler = do
  let Route queueName = route
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       forkIO $ do
         let rawReqMsg = amqpMsgToString msg
         case readMaybe rawReqMsg of
           Nothing -> return ()
           Just RequestMessage {id = requestId, responseQueue, req} -> do
             let Id _responseQueue = responseQueue
             let publish res = do
                   let msgBody =
                         ByteString.Lazy.UTF8.fromString $
                         show $ ResponseMessage {requestId, res}
                   AMQP.publishMsg
                     chan
                     "" -- Default exchange just sends message to queue specified by routing key.
                     _responseQueue -- Exchange routing key.
                     AMQP.newMsg {AMQP.msgBody}
                   return ()
             case readMaybe $ Text.unpack req of
               Nothing ->
                 publish . Left . BadCall $ Text.append "bad input: " req
               Just r -> handler r >>= publisher (publish . Right)
         AMQP.ackEnv env
       return ())
  return ()

-- Direct RPC server.
serveRPC :: (Read req, Show res) => T -> Route -> (req -> IO res) -> IO ()
serveRPC = _serveRPC (\publish res -> publish $ Result (show res))

-- Streaming RPC server.
serveRPC' ::
     (Read req, Show res)
  => T
  -> Route
  -> (req -> IO (Streamly.Serial res))
  -> IO ()
serveRPC' =
  _serveRPC
    (\publish results -> do
       Streamly.mapM_ (publish . Result . show) results
       publish EndOfResults)

-- General RPC call.
-- handler :: IO (responseHandler, result, done) defines how to wrap results
-- up for the caller. Registers a handler in the waiting RPC call hash table,
-- and deregisters it after done resolves. Times out if done is not fulfilled
-- after _timeout, with the timeout reset every time a response is received.
_callRPCTimeout ::
     (Show req)
  => IO (Response -> IO (), IO res, IO ())
  -- Response handler, result, done.
  -> DiffTime
  -> T
  -> Route
  -> req
  -> IO res
_callRPCTimeout handler _timeout T {chan, responseQueue, waitingCalls} route req = do
  let Route queueName = route
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg = RequestMessage {id, responseQueue, req = show req}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (push, result, waitForDone) <- handler
  renewTimeout <- timeoutThrowIO' waitForDone _timeout
  HashTable.insert waitingCalls id (\x -> renewTimeout >> push x)
  _ <- AMQP.publishMsg chan "" queueName AMQP.newMsg {AMQP.msgBody}
  forkIO $ waitForDone >> HashTable.delete waitingCalls id
  result

-- Direct RPC call
callRPCTimeout ::
     forall req res. (Show req, Read res)
  => DiffTime
  -> T
  -> Route
  -> req
  -> IO res
callRPCTimeout =
  _callRPCTimeout
    (do resultVar <- MVar.newEmptyMVar
        let push =
              \case
                Result res ->
                  MVar.putMVar resultVar (read @res (Text.unpack res))
                EndOfResults ->
                  throwIO $
                  InternalBridgeException
                    "Direct RPC should never get EndOfResults"
        let result = MVar.readMVar resultVar
        return (push, result, void result))

-- Streaming RPC call
callRPCTimeout' ::
     forall req res. (Show req, Read res)
  => DiffTime
  -> T
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout' =
  _callRPCTimeout
    (do (streamPush, results) <- repeatableStream
        let push =
              \case
                Result res -> streamPush (Just (read @res (Text.unpack res)))
                EndOfResults -> streamPush Nothing
        let waitForDone = void (Streamly.mapM_ return results)
        return (push, return results, waitForDone))

-- Direct RPC call with default timeout.
callRPC :: (Show req, Read res) => T -> Route -> req -> IO res
callRPC = callRPCTimeout defaultTimeout

-- Streaming RPC call with default timeout.
callRPC' ::
     (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC' = callRPCTimeout' defaultTimeout

publish :: (Show item) => T -> Id -> item -> IO ()
publish T {chan} route item = do
  let Id _route = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName = _route, AMQP.exchangeType = "fanout"}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show item
  AMQP.publishMsg chan _route "" AMQP.newMsg {AMQP.msgBody}
  return ()
-- subscribe :: (Read item) => T -> Id -> IO (Streamly.Serial item)
-- subscribe T {chan} route = do
--   AMQP.declareExchange chan AMQP.newExchange {exchangeName = route, exchangeType = "fanout"}
--   -- Sanity check to make sure the desired pub/sub route is actually a fanout exchange.
--   queueName <- generateNewQueueName
--   AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
--   AMQP.bindQueue chan queueName route ""
--   (push, results) <- repeatableStream
--   AMQP.consumeMsgs chan queueName AMQP.NoAck (\(msg, _) -> do
--     mapM push (readMaybe msg))
