module Bridge
  ( T(..)
  , Config(..)
  , localConfig
  , with
  , withForever
  , defaultTimeout
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

data Config a b c = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  , publishAPI :: Proxy a
  , serveAPI :: Proxy b
  , handlers :: Butler.Server b
  , serveAPI' :: Proxy c
  , handlers' :: Butler.Server' c
  }

makeConfig ::
     Text
  -> Text
  -> Text
  -> Text
  -> Proxy a
  -> Proxy b
  -> Butler.Server b
  -> Proxy c
  -> Butler.Server' c
  -> Config a b c
makeConfig hostname virtualHost username password publishAPI serveAPI handlers serveAPI' handlers' =
  Config
    { hostname
    , virtualHost
    , username
    , password
    , publishAPI
    , serveAPI
    , handlers
    , serveAPI'
    , handlers'
    }

localConfig ::
     Proxy a
  -> Proxy b
  -> Butler.Server b
  -> Proxy c
  -> Butler.Server' c
  -> Config a b c
localConfig = makeConfig "localhost" "/" "guest" "guest"

data T a b c = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Id
  , responseConsumerTag :: AMQP.ConsumerTag
  , serveConsumerTags :: [AMQP.ConsumerTag]
  , serveConsumerTags' :: [AMQP.ConsumerTag]
  , responseHandlers :: HashTable Id (Response -> IO ())
  -- Key: subscriberId to Value: (consumerTag, close)
  , subscriberInfo :: HashTable Id (AMQP.ConsumerTag, IO ())
  , publishers :: Butler.Publisher a
  , publishers' :: Butler.Publisher' a
  , subscribers' :: Butler.Subscriber' a
  , clients :: Butler.Client b
  , clients' :: Butler.Client' c
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

amqpMsgToText :: AMQP.Message -> Text
amqpMsgToText = Text.pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

readAmqpMsg :: (Read a) => AMQP.Message -> Maybe a
readAmqpMsg = Text.Read.readMaybe . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: (Show a) => a -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString (show x)}

with ::
     ( Butler.HasPublisher a
     , Butler.HasSubscriber a
     , Butler.HasServer b
     , Butler.HasClient b
     , Butler.HasServer c
     , Butler.HasClient c
     )
  => Config a b c
  -> (T a b c -> IO ())
  -> IO ()
with config = bracket (make config) kill

withForever ::
     ( Butler.HasPublisher a
     , Butler.HasSubscriber a
     , Butler.HasServer b
     , Butler.HasClient b
     , Butler.HasServer c
     , Butler.HasClient c
     )
  => Config a b c
  -> (T a b c -> IO ())
  -> IO ()
withForever config action =
  bracket (make config) kill (\x -> action x >> blockForever)

-- Connects to bridge, begins listening on RPC queue.
-- Returns (state, publishers, publishers', subscribers', clients, clients').
make ::
     ( Butler.HasPublisher a
     , Butler.HasSubscriber a
     , Butler.HasServer b
     , Butler.HasClient b
     , Butler.HasServer c
     , Butler.HasClient c
     )
  => Config a b c
  -> IO (T a b c)
make Config { hostname
            , virtualHost
            , username
            , password
            , publishAPI
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
  serveConsumerTags <- Butler.makeServer serveAPI handlers (serveRPC chan)
  serveConsumerTags' <- Butler.makeServer' serveAPI' handlers' (serveRPC' chan)
  subscriberInfo <- HashTable.new
  let publishers = Butler.makePublisher publishAPI (publish chan)
  let publishers' = Butler.makePublisher' publishAPI (publish chan)
  let subscribers' =
        Butler.makeSubscriber' publishAPI (subscribe' (chan, subscriberInfo))
  let clients =
        Butler.makeClient
          serveAPI
          (callRPCTimeout (chan, responseQueue, responseHandlers))
  let clients' =
        Butler.makeClient'
          serveAPI'
          (callRPCTimeout' (chan, responseQueue, responseHandlers))
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
      , publishers
      , publishers'
      , subscribers'
      , clients
      , clients'
      }

-- Kills and cleans up bridge.
kill :: T a b c -> IO ()
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
  HashTable.mapM_ (unsubscribe t . fst) subscriberInfo -- TODO: Fix.
  -- responseHandlers will automatically time out.
  _ <- AMQP.deleteQueue chan _responseQueue
  AMQP.closeConnection conn -- And chan.

-- RPC server.
_serveRPC ::
     ((Response -> IO ()) -> x' -> IO ())
  -> AMQP.Channel
  -> Route
  -> (Text -> IO x')
  -> IO AMQP.ConsumerTag
_serveRPC publisher chan (Route queueName) handler = do
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
serveRPC :: AMQP.Channel -> Route -> (Text -> IO Text) -> IO AMQP.ConsumerTag
serveRPC = _serveRPC (\pub -> pub . Result)

-- Streaming RPC server.
serveRPC' ::
     AMQP.Channel
  -> Route
  -> (Text -> IO (Streamly.Serial Text))
  -> IO AMQP.ConsumerTag
serveRPC' =
  _serveRPC
    (\pub results -> do
       Streamly.mapM_ (pub . Result) results
       pub EndOfResults)

-- General RPC call.
-- handler :: IO (responseHandler, result, done) defines how to wrap results up for the caller.
-- Registers a handler in the waiting RPC call hash table, and deregisters it after done resolves.
-- Times out if done is not fulfilled after _timeout, with the timeout reset every time a response
-- is received.
_callRPCTimeout ::
     IO (Response -> IO (), IO x', IO ())
  -- Response handler, result, done.
  -> (AMQP.Channel, Id, HashTable Id (Response -> IO ()))
  -- Channel, response queue ID, response handler table.
  -> Time Second
  -> Route
  -> Text
  -> IO x'
_callRPCTimeout handler (chan, responseQueue, responseHandlers) _timeout (Route queueName) req = do
  id <- newUuid
  let request = toAmqpMsg RequestMessage {id, responseQueue, req = show req}
  (push, result, waitForDone) <- handler
  renewTimeout <- timeoutThrow' waitForDone _timeout
  HashTable.insert responseHandlers id (\x -> renewTimeout >> push x)
  _ <- AMQP.publishMsg chan "" queueName request
  async $ waitForDone >> HashTable.delete responseHandlers id
  result

-- Direct RPC call.
callRPCTimeout ::
     (AMQP.Channel, Id, HashTable Id (Response -> IO ()))
  -> Time Second
  -> Route
  -> Text
  -> IO Text
callRPCTimeout =
  _callRPCTimeout
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
callRPCTimeout' ::
     (AMQP.Channel, Id, HashTable Id (Response -> IO ()))
  -> Time Second
  -> Route
  -> Text
  -> IO (Streamly.Serial Text)
callRPCTimeout' =
  _callRPCTimeout
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
publish :: AMQP.Channel -> Route -> Text -> IO ()
publish chan route a = do
  let Route exchangeName = route
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
subscribe' ::
     (AMQP.Channel, HashTable Id (AMQP.ConsumerTag, IO ()))
  -> Route
  -> IO (Id, Streamly.Serial Text)
subscribe' (chan, subscriberInfo) (Route route) = do
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName = route, AMQP.exchangeType = "fanout"}
  -- Sanity check to make sure the desired pub/sub route is actually a fanout exchange.
  queueName <- newQueueName
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.bindQueue chan queueName route "" -- Routing key blank.
  (push, close, results) <- repeatableStream
  consumerTag <-
    AMQP.consumeMsgs chan queueName AMQP.NoAck (push . amqpMsgToText . fst)
  let subscriberId = Id queueName
  HashTable.insert subscriberInfo subscriberId (consumerTag, close)
  return (subscriberId, results)

-- Unsubscribes a consumer (idempotent).
unsubscribe :: T a b c -> Id -> IO ()
unsubscribe T {chan, subscriberInfo} subscriberId = do
  let Id queueName = subscriberId
  HashTable.lookup subscriberInfo subscriberId >>= \case
    Nothing -> return ()
    Just (consumerTag, close) -> do
      AMQP.cancelConsumer chan consumerTag
      AMQP.deleteQueue chan queueName
      close
  HashTable.delete subscriberInfo subscriberId

unsubscribe' :: T a b c -> Id -> IO ()
unsubscribe' = unsubscribe
