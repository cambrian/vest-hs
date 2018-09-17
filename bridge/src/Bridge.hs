module Bridge
  ( T(..)
  , Config(..)
  , localConfig
  , with
  , withForever
  , serveRPC
  , serveRPC'
  , callRPC
  , callRPC'
  , defaultTimeout
  , callRPCTimeout
  , callRPCTimeout'
  , publish
  , publish'
  , subscribe'
  , unsubscribe
  , unsubscribe'
  , BridgeException(..)
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified Text.Read
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

localConfig :: Config
localConfig =
  Config
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
  , responseHandlers :: HashTable Id (Response -> IO ())
  -- Key: subscriberId to Value: (consumerTag, close)
  , subscribers :: HashTable Id (AMQP.ConsumerTag, IO ())
  }

data BridgeException
  = BadCall Text
  | InternalBridgeException Text -- If you get one of these, file a bug report.
  deriving ( Eq
           , Ord
           , Show
           , Read
           , Typeable
           , Generic
           , Exception
           , Hashable
           , FromJSON
           , ToJSON
           )

newQueueName :: IO Text
newQueueName = do
  (Id id) <- newUuid
  myHostName <- Network.HostName.getHostName >>- Text.pack
  return $ myHostName <> "." <> id

readAmqpMsg :: (Read a) => AMQP.Message -> Maybe a
readAmqpMsg = Text.Read.readMaybe . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: (Show a) => a -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString (show x)}

with :: Config -> (T -> IO ()) -> IO ()
with config = bracket (make config) kill

withForever :: Config -> (T -> IO ()) -> IO ()
withForever config action =
  bracket (make config) kill (\x -> action x >> blockForever)

-- Connects to bridge, begins listening on RPC queue.
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  queueName <- newQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  responseHandlers <- HashTable.new
  let handleResponse ResponseMessage {requestId, res} =
        case res of
          Left exception -> throw exception
          Right r -> HashTable.lookup responseHandlers requestId >>= mapM_ ($ r)
  responseConsumerTag <-
    AMQP.consumeMsgs
      chan
      queueName
      AMQP.Ack
      (\(msg, env) -> do
         readAmqpMsg msg >|>| handleResponse -- Do nothing if response message is malformed.
         AMQP.ackEnv env)
  serveConsumerTags <- HashTable.new
  subscribers <- HashTable.new
  return
    T
      { conn
      , chan
      , responseQueue = Id queueName
      , responseConsumerTag
      , serveConsumerTags
      , responseHandlers
      , subscribers
      }

-- Kills and cleans up bridge.
kill :: T -> IO ()
kill t = do
  let T { conn
        , chan
        , responseQueue
        , responseConsumerTag
        , serveConsumerTags
        , subscribers
        } = t
      Id _responseQueue = responseQueue
  AMQP.cancelConsumer chan responseConsumerTag
  HashTable.mapM_ (AMQP.cancelConsumer chan . snd) serveConsumerTags
  HashTable.mapM_ (unsubscribe t . fst) subscribers
  -- responseHandlers will automatically time out.
  _ <- AMQP.deleteQueue chan _responseQueue
  AMQP.closeConnection conn -- And chan.

-- RPC server.
_serveRPC ::
     (Read req)
  => ((Response -> IO ()) -> res' -> IO ())
  -> T
  -> Route
  -> (req -> IO res')
  -> IO ()
_serveRPC publisher T {chan} (Route queueName) handler = do
  let handleCall RequestMessage {id = requestId, responseQueue, req} = do
        let Id _responseQueue = responseQueue
            pub res =
              void $
              AMQP.publishMsg
                chan
                "" -- Default exchange just sends message to queue specified by routing key.
                _responseQueue -- Exchange routing key.
                (toAmqpMsg ResponseMessage {requestId, res})
        case readMaybe req of
          Nothing -> pub . Left . BadCall $ "bad input: " <> req
          Just r -> handler r >>= publisher (pub . Right)
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) ->
       void . async $ do
         readAmqpMsg msg >|>| handleCall -- Do nothing if request message is malformed.
         AMQP.ackEnv env)
  return ()

-- Direct RPC server.
serveRPC :: (Read req, Show res) => T -> Route -> (req -> IO res) -> IO ()
serveRPC = _serveRPC (\pub res -> pub $ Result (show res))

-- Streaming RPC server.
serveRPC' ::
     (Read req, Show res)
  => T
  -> Route
  -> (req -> IO (Streamly.Serial res))
  -> IO ()
serveRPC' =
  _serveRPC
    (\pub results -> do
       Streamly.mapM_ (pub . Result . show) results
       pub EndOfResults)

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
_callRPCTimeout handler _timeout T {chan, responseQueue, responseHandlers} (Route queueName) req = do
  id <- newUuid
  let request = toAmqpMsg RequestMessage {id, responseQueue, req = show req}
  (push, result, waitForDone) <- handler
  renewTimeout <- timeoutThrow' waitForDone _timeout
  HashTable.insert responseHandlers id (\x -> renewTimeout >> push x)
  _ <- AMQP.publishMsg chan "" queueName request
  async $ waitForDone >> HashTable.delete responseHandlers id
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
                  throw $
                  InternalBridgeException
                    "direct RPC should never get EndOfResults"
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
    (do (_push, close, results) <- repeatableStream
        let push =
              \case
                Result res -> readUnsafe @res res >>= _push
                EndOfResults -> close
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
publish :: (Show a) => T -> Route -> a -> IO ()
publish T {chan} route a = do
  let Route exchangeName = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
  AMQP.publishMsg chan exchangeName "" (toAmqpMsg a) -- Queue name blank.
  return ()

publish' :: (Show a) => T -> Route -> Streamly.Serial a -> IO ()
publish' T {chan} route as = do
  let Route exchangeName = route
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
  Streamly.mapM_ (AMQP.publishMsg chan exchangeName "" . toAmqpMsg) as

-- Subscribe (non-streaming version) is deliberately unimplemented, because RabbitMQ does not
-- support message history. Candidate solutions are building a separate pub/sub system on Kafka (or
-- similar), or adding Cassandra to RabbitMQ. For now, you can use makeStreamVar in conjunction
-- with subscribe' to get one-off values from a published topic.
-- Returns subscriber ID and result stream as tuple (ID, stream).
subscribe' ::
     forall a. (Read a)
  => T
  -> Route
  -> IO (Id, Streamly.Serial a)
subscribe' T {chan, subscribers} (Route route) = do
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName = route, AMQP.exchangeType = "fanout"}
  -- Sanity check to make sure the desired pub/sub route is actually a fanout exchange.
  queueName <- newQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.bindQueue chan queueName route "" -- Routing key blank.
  (push, close, results) <- repeatableStream
  consumerTag <-
    AMQP.consumeMsgs chan queueName AMQP.NoAck (mapM_ push . readAmqpMsg . fst)
  let subscriberId = Id queueName
  HashTable.insert subscribers subscriberId (consumerTag, close)
  return (subscriberId, results)

-- Unsubscribes a consumer (idempotent).
unsubscribe :: T -> Id -> IO ()
unsubscribe T {chan, subscribers} subscriberId = do
  let Id queueName = subscriberId
  HashTable.lookup subscribers subscriberId >>= \case
    Nothing -> return ()
    Just (consumerTag, close) -> do
      AMQP.cancelConsumer chan consumerTag
      AMQP.deleteQueue chan queueName
      close
  HashTable.delete subscribers subscriberId

unsubscribe' :: T -> Id -> IO ()
unsubscribe' = unsubscribe
