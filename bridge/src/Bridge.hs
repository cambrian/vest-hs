module Bridge
  ( T(..)
  , Config(..)
  , BridgeException(..)
  , localConfig
  , defaultTimeout
  , with
  , withForever
  , serveRPC
  , serveRPC'
  , callRPC
  , callRPC'
  , callRPCTimeout
  , callRPCTimeout'
  , publish
  , subscribe
  , unsubscribe
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Foreign.StablePtr as StablePtr
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

localConfig :: Bridge.Config
localConfig =
  Bridge.Config
    { hostname = "localhost"
    , virtualHost = "/"
    , username = "guest"
    , password = "guest"
    }

data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Id
  , responseConsumerTag :: AMQP.ConsumerTag
  , serveConsumerTags :: HashTable Route AMQP.ConsumerTag
  , callHandlers :: HashTable Id (Response -> IO ())
  , unsubscribeHandlers :: HashTable Id (IO ())
  }

data BridgeException
  = BadCall Text
  | InternalBridgeException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception BridgeException

instance Hashable BridgeException

amqpMsgToText :: AMQP.Message -> Text
amqpMsgToText = Text.pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: (Show a) => a -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString (show x)}

newQueueName :: IO Text
newQueueName = do
  uuid <- UUID.nextRandom
  myHostName <- Network.HostName.getHostName
  return $ Text.pack (myHostName ++ "." ++ UUID.toString uuid)

with :: Config -> (T -> IO ()) -> IO ()
with config = bracket (make config) kill

withForever :: Config -> (T -> IO ()) -> IO ()
withForever config =
  bracket
    (do make config
        myThreadId >>= StablePtr.newStablePtr
        STM.atomically STM.retry)
    kill

-- Connects to bridge, begins listening on RPC queue.
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  queueName <- newQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  callHandlers <- HashTable.new
  responseConsumerTag <-
    AMQP.consumeMsgs
      chan
      queueName
      AMQP.Ack
      (\(msg, env) -> do
         case readMaybe $ amqpMsgToText msg of
           Nothing -> return () -- Swallow if entire message is garbled.
           Just ResponseMessage {requestId, res} ->
             case res of
               Left exception -> throwIO exception
               Right r ->
                 (HashTable.lookup callHandlers requestId) >>= mapM_ ($ r)
         AMQP.ackEnv env)
  serveConsumerTags <- HashTable.new
  unsubscribeHandlers <- HashTable.new
  return
    T
      { conn
      , chan
      , responseQueue = Id queueName
      , responseConsumerTag
      , serveConsumerTags
      , callHandlers
      , unsubscribeHandlers
      }

-- Kills and cleans up bridge.
kill :: T -> IO ()
kill T { conn
       , chan
       , responseQueue
       , responseConsumerTag
       , serveConsumerTags
       , unsubscribeHandlers
       } = do
  let Id _responseQueue = responseQueue
  AMQP.cancelConsumer chan responseConsumerTag
  HashTable.mapM_
    (\(route, tag) -> AMQP.cancelConsumer chan tag)
    serveConsumerTags
  HashTable.mapM_
    (\(subscriberId, unsubscribe) -> unsubscribe)
    unsubscribeHandlers
  -- callHandlers will automatically time out.
  _ <- AMQP.deleteQueue chan _responseQueue
  AMQP.closeConnection conn -- And chan.

-- RPC server.
_serveRPC ::
     (Read req)
  => ((Response -> IO ()) -> res -> IO ())
  -> T
  -> Route
  -> (req -> IO res)
  -> IO ()
_serveRPC publisher T {chan} (Route queueName) handler = do
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       forkIO $ do
         case readMaybe $ amqpMsgToText msg of
           Nothing -> return ()
           Just RequestMessage {id = requestId, responseQueue, req} -> do
             let Id _responseQueue = responseQueue
             let doPublish res =
                   void $
                   AMQP.publishMsg
                     chan
                     "" -- Default exchange just sends message to queue specified by routing key.
                     _responseQueue -- Exchange routing key.
                     (toAmqpMsg ResponseMessage {requestId, res})
             case readMaybe req of
               Nothing ->
                 doPublish . Left . BadCall $ Text.append "bad input: " req
               Just r -> handler r >>= publisher (doPublish . Right)
         AMQP.ackEnv env
       return ())
  return ()

-- Direct RPC server.
serveRPC :: (Read req, Show res) => T -> Route -> (req -> IO res) -> IO ()
serveRPC = _serveRPC (\doPublish res -> doPublish $ Result (show res))

-- Streaming RPC server.
serveRPC' ::
     (Read req, Show res)
  => T
  -> Route
  -> (req -> IO (Streamly.Serial res))
  -> IO ()
serveRPC' =
  _serveRPC
    (\doPublish results -> do
       Streamly.mapM_ (doPublish . Result . show) results
       doPublish EndOfResults)

-- General RPC call.
-- handler :: IO (responseHandler, result, done) defines how to wrap results
-- up for the caller. Registers a handler in the waiting RPC call hash table,
-- and deregisters it after done resolves. Times out if done is not fulfilled
-- after _timeout, with the timeout reset every time a response is received.
_callRPCTimeout ::
     (Show req)
  => IO (Response -> IO (), IO res, IO ())
  -- Response handler, result, done.
  -> Time Second
  -> T
  -> Route
  -> req
  -> IO res
_callRPCTimeout handler _timeout T {chan, responseQueue, callHandlers} (Route queueName) req = do
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let request = toAmqpMsg RequestMessage {id, responseQueue, req = show req}
  (push, result, waitForDone) <- handler
  renewTimeout <- timeoutThrowIO' waitForDone _timeout
  HashTable.insert callHandlers id (\x -> renewTimeout >> push x)
  _ <- AMQP.publishMsg chan "" queueName request
  forkIO $ waitForDone >> HashTable.delete callHandlers id
  result

-- Direct RPC call
callRPCTimeout ::
     forall req res. (Show req, Read res)
  => Time Second
  -> T
  -> Route
  -> req
  -> IO res
callRPCTimeout =
  _callRPCTimeout
    (do resultVar <- MVar.newEmptyMVar
        let push =
              \case
                Result res -> readUnsafe @res res >>= MVar.putMVar resultVar
                EndOfResults ->
                  throwIO $
                  InternalBridgeException
                    "Direct RPC should never get EndOfResults"
            result = MVar.readMVar resultVar
        return (push, result, void result))

-- Streaming RPC call
callRPCTimeout' ::
     forall req res. (Show req, Read res)
  => Time Second
  -> T
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout' =
  _callRPCTimeout
    (do (streamPush, results) <- repeatableStream
        let push =
              \case
                Result res -> readUnsafe @res res >>= streamPush . Just
                EndOfResults -> streamPush Nothing
            done = Streamly.mapM_ return results
        return (push, return results, done))

defaultTimeout :: Time Second
defaultTimeout = sec 10

-- Direct RPC call with default timeout.
callRPC :: (Show req, Read res) => T -> Route -> req -> IO res
callRPC = callRPCTimeout defaultTimeout

-- Streaming RPC call with default timeout.
callRPC' ::
     (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC' = callRPCTimeout' defaultTimeout

-- Publish in the usual pub/sub model.
publish :: (Show item) => T -> Route -> item -> IO ()
publish T {chan} route item = do
  let Route exchangeName = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
  AMQP.publishMsg chan exchangeName "" (toAmqpMsg item) -- Queue name blank.
  return ()

publish' :: (Show item) => T -> Route -> Streamly.Serial item -> IO ()
publish' T {chan} route itemStream = do
  let Route exchangeName = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
  Streamly.mapM_
    (\item -> do
       (AMQP.publishMsg chan exchangeName "") . toAmqpMsg $ item
       return () -- Queue name blank.
     )
    itemStream

-- Returns subscriber ID and result stream as tuple (ID, stream).
subscribe :: (Read item) => T -> Id -> IO (Id, Streamly.Serial item)
subscribe T {chan, unsubscribeHandlers} route = do
  let Id _route = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName = _route, AMQP.exchangeType = "fanout"}
  -- Sanity check to make sure the desired pub/sub route is actually a fanout exchange.
  queueName <- newQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.bindQueue chan queueName _route "" -- Routing key blank.
  (push, results) <- repeatableStream
  consumerTag <-
    AMQP.consumeMsgs
      chan
      queueName
      AMQP.NoAck
      (\(msg, _) -> mapM_ (push . Just) (msg & amqpMsgToText & readMaybe))
  let subscriberId = Id queueName
  HashTable.insert
    unsubscribeHandlers
    subscriberId
    (do AMQP.cancelConsumer chan consumerTag
        AMQP.deleteQueue chan queueName
        push Nothing)
  return (subscriberId, results)

-- Unsubscribes a consumer (idempotent).
unsubscribe :: T -> Id -> IO ()
unsubscribe T {chan, unsubscribeHandlers} subscriberId = do
  unsubscribeMaybe <- HashTable.lookup unsubscribeHandlers subscriberId
  case unsubscribeMaybe of
    Nothing -> return ()
    Just unsubscribe -> unsubscribe
