module Bridge
  ( T(..)
  , Config(..)
  , makeConfig
  , localConfig
  , with
  , withForever
  , callUnsafe
  , callUnsafe'
  , defaultTimeout
  , publishUnsafe
  , subscribeUnsafe'
  , unsubscribe
  , unsubscribe'
  , BridgeException(..)
  ) where

import qualified Butler
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

data Config a b = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  , serveAPI :: Proxy a
  , handlers :: Butler.Server a
  , serveAPI' :: Proxy b
  , handlers' :: Butler.Server' b
  }

makeConfig ::
     Text
  -> Text
  -> Text
  -> Text
  -> Proxy a
  -> Butler.Server a
  -> Proxy b
  -> Butler.Server' b
  -> Config a b
makeConfig hostname virtualHost username password serveAPI handlers serveAPI' handlers' =
  Config
    { hostname
    , virtualHost
    , username
    , password
    , serveAPI
    , handlers
    , serveAPI'
    , handlers'
    }

localConfig ::
     Proxy a -> Butler.Server a -> Proxy b -> Butler.Server' b -> Config a b
localConfig = makeConfig "localhost" "/" "guest" "guest"

data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Id
  , responseConsumerTag :: AMQP.ConsumerTag
  , serveConsumerTags :: [AMQP.ConsumerTag]
  , serveConsumerTags' :: [AMQP.ConsumerTag]
  , responseHandlers :: HashTable Id (Response -> IO ())
  -- Key: subscriberId to Value: (consumerTag, close)
  , subscriberInfo :: HashTable Id (AMQP.ConsumerTag, IO ())
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

with ::
     (Butler.HasServer a, Butler.HasServer b)
  => Config a b
  -> (T -> IO ())
  -> IO ()
with config = bracket (make config) kill

withForever ::
     (Butler.HasServer a, Butler.HasServer b)
  => Config a b
  -> (T -> IO ())
  -> IO ()
withForever config action =
  bracket (make config) kill (\x -> action x >> blockForever)

-- Connects to bridge, begins listening on RPC queue.
-- Returns (state, publishers, publishers', subscribers', clients, clients').
make :: (Butler.HasServer a, Butler.HasServer b) => Config a b -> IO T
make Config { hostname
            , virtualHost
            , username
            , password
            , serveAPI
            , handlers
            , serveAPI'
            , handlers'
            } = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  queueName <- newQueueName
  let responseQueue = Id queueName
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
  serveConsumerTags <- Butler.makeServer serveAPI handlers (serve chan)
  serveConsumerTags' <- Butler.makeServer' serveAPI' handlers' (serve' chan)
  subscriberInfo <- HashTable.new
  return
    T
      { conn
      , chan
      , responseQueue
      , responseConsumerTag
      , serveConsumerTags
      , serveConsumerTags'
      , responseHandlers
      , subscriberInfo
      }

-- Kills and cleans up bridge.
kill :: T -> IO ()
kill t = do
  let T { conn
        , chan
        , responseQueue
        , responseConsumerTag
        , serveConsumerTags
        , serveConsumerTags'
        , subscriberInfo
        } = t
      Id _responseQueue = responseQueue
  AMQP.cancelConsumer chan responseConsumerTag
  mapM_ (AMQP.cancelConsumer chan) serveConsumerTags
  mapM_ (AMQP.cancelConsumer chan) serveConsumerTags'
  HashTable.mapM_ (unsubscribe t . fst) subscriberInfo
  -- responseHandlers will automatically time out.
  _ <- AMQP.deleteQueue chan _responseQueue
  AMQP.closeConnection conn -- And chan.

-- RPC server.
_serve ::
     ((Response -> IO ()) -> x' -> IO ())
  -> AMQP.Channel
  -> Route
  -> (Text -> IO x')
  -> IO AMQP.ConsumerTag
_serve publisher chan (Route queueName) handler = do
  let wrappedHandler RequestMessage {id = requestId, responseQueue, req} = do
        let Id _responseQueue = responseQueue
            pub res =
              void $
              AMQP.publishMsg
                chan
                "" -- Default exchange just sends message to queue specified by routing key.
                _responseQueue -- Exchange routing key.
                (toAmqpMsg ResponseMessage {requestId, res})
        catch
          (handler req >>= publisher (pub . Right))
          (\e -> do
             let _ = (e :: ReadException)
             pub . Left . BadCall $ "bad input: " <> req)
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  handlerTag <-
    AMQP.consumeMsgs
      chan
      queueName
      AMQP.Ack
      (\(msg, env) ->
         void . async $ do
           readAmqpMsg msg >|>| wrappedHandler -- Do nothing if request message is malformed.
           AMQP.ackEnv env)
  return handlerTag

-- Direct RPC server.
serve :: AMQP.Channel -> Route -> (Text -> IO Text) -> IO AMQP.ConsumerTag
serve = _serve (\pub -> pub . Result)

-- Streaming RPC server.
serve' ::
     AMQP.Channel
  -> Route
  -> (Text -> IO (Streamly.Serial Text))
  -> IO AMQP.ConsumerTag
serve' =
  _serve
    (\pub results -> do
       Streamly.mapM_ (pub . Result) results
       pub EndOfResults)

-- General RPC call.
-- handler :: IO (responseHandler, result, done) defines how to wrap results up for the caller.
-- Registers a handler in the waiting RPC call hash table, and deregisters it after done resolves.
-- Times out if done is not fulfilled after _timeout, with the timeout reset every time a response
-- is received.
_call ::
     IO (Response -> IO (), IO x', IO ())
  -- Response handler, result, done.
  -> Time Second
  -> T
  -> Route
  -> Text
  -> IO x'
_call handler _timeout T {chan, responseQueue, responseHandlers} (Route queueName) req = do
  id <- newUuid
  let request = toAmqpMsg RequestMessage {id, responseQueue, req}
  (push, result, waitForDone) <- handler
  renewTimeout <- timeoutThrow' waitForDone _timeout
  HashTable.insert responseHandlers id (\x -> renewTimeout >> push x)
  _ <- AMQP.publishMsg chan "" queueName request
  async $ waitForDone >> HashTable.delete responseHandlers id
  result

-- Direct RPC call.
callUnsafe :: Time Second -> T -> Route -> Text -> IO Text
callUnsafe =
  _call
    (do resultVar <- MVar.newEmptyMVar
        let push =
              \case
                Result res -> MVar.putMVar resultVar res
                EndOfResults ->
                  throw $
                  InternalBridgeException
                    "direct RPC should never get EndOfResults"
            result = MVar.readMVar resultVar
        return (push, result, void result))

-- Streaming RPC call.
callUnsafe' :: Time Second -> T -> Route -> Text -> IO (Streamly.Serial Text)
callUnsafe' =
  _call
    (do (_push, close, results) <- repeatableStream
        let push =
              \case
                Result res -> _push res
                EndOfResults -> close
            done = Streamly.mapM_ return results
        return (push, return results, done))

defaultTimeout :: Time Second
defaultTimeout = sec 10

-- Publish in the usual pub/sub model.
publishUnsafe :: T -> Route -> Text -> IO ()
publishUnsafe T {chan} (Route exchangeName) a = do
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
  AMQP.publishMsg chan exchangeName "" (toAmqpMsg a) -- Queue name blank.
  return ()

-- Subscribe (non-streaming version) is deliberately unimplemented, because RabbitMQ does not
-- support message history. Candidate solutions are building a separate pub/sub system on Kafka (or
-- similar), or adding Cassandra to RabbitMQ. For now, you can use makeStreamVar in conjunction
-- with subscribe' to get one-off values from a published topic.
-- Returns subscriber ID and result stream as tuple (ID, stream).
subscribeUnsafe' :: T -> Route -> IO (Id, Streamly.Serial Text)
subscribeUnsafe' T {chan, subscriberInfo} (Route route) = do
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
  HashTable.insert subscriberInfo subscriberId (consumerTag, close)
  return (subscriberId, results)

-- Unsubscribes a consumer (idempotent).
unsubscribe :: T -> Id -> IO ()
unsubscribe T {chan, subscriberInfo} subscriberId = do
  let Id queueName = subscriberId
  HashTable.lookup subscriberInfo subscriberId >>= \case
    Nothing -> return ()
    Just (consumerTag, close) -> do
      AMQP.cancelConsumer chan consumerTag
      AMQP.deleteQueue chan queueName
      close
  HashTable.delete subscriberInfo subscriberId

unsubscribe' :: T -> Id -> IO ()
unsubscribe' = unsubscribe
