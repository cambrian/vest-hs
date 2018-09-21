module Bridge.Transports.Amqp
  ( T(..)
  , Config(..)
  , localConfig
  ) where

import Bridge.PubSub
import Bridge.Rpc
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: Id "RpcRequest"
  , responseQueue :: Id "ResponseQueue"
    -- Other metadata (time?).
  , reqText :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id "RpcRequest"
    -- Other metadata.
  , response :: Either RpcClientException Text
  } deriving (Eq, Show, Read)

data Config = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  }

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
  , responseQueue :: Id "ResponseQueue"
  , responseConsumerTag :: AMQP.ConsumerTag
  , servedRouteTags :: HashTable (Id "Rpc") AMQP.ConsumerTag
  , responseHandlers :: HashTable (Id "RpcRequest") (Text -> IO ())
  , publishedTopics :: HashTable (Id "Topic") ()
  , subscriberInfo :: HashTable (Id "Subscriber") (AMQP.ConsumerTag, IO ())
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Id id) <- newUuid
  myHostName <- Network.HostName.getHostName >>- pack
  return $ myHostName <> "." <> id

fromAmqpMsg :: (Text -> a) -> AMQP.Message -> a
fromAmqpMsg deserialize =
  deserialize . pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: (a -> Text) -> a -> AMQP.Message
toAmqpMsg serialize x =
  AMQP.newMsg
    {AMQP.msgBody = ByteString.Lazy.UTF8.fromString . unpack . serialize $ x}

_unsubscribe :: T -> Id "Subscriber" -> IO ()
_unsubscribe T {chan, subscriberInfo} subscriberId = do
  let Id queueName = subscriberId
      maybeUnsubscribe =
        (>|>| \(consumerTag, close) -> do
                AMQP.cancelConsumer chan consumerTag
                AMQP.deleteQueue chan queueName
                close)
  HashTable.lookup subscriberInfo subscriberId >>= maybeUnsubscribe
  HashTable.delete subscriberInfo subscriberId

type instance ResourceConfig T = Config

instance Resource T where
  hold :: Config -> IO T
    -- Connects to bridge, begins listening on RPC queue.
  hold Config {hostname, virtualHost, username, password} = do
    conn <- AMQP.openConnection (unpack hostname) virtualHost username password
    chan <- AMQP.openChannel conn
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    responseHandlers <- HashTable.new
    let handleResponse ResponseMessage {requestId, response} =
          case response of
            Left exception -> throw exception
            Right text ->
              HashTable.lookup responseHandlers requestId >>= mapM_ ($ text)
    responseConsumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           fromAmqpMsg read msg >|>| handleResponse -- Do nothing if response message is malformed.
           AMQP.ackEnv env)
    servedRouteTags <- HashTable.new
    publishedTopics <- HashTable.new
    subscriberInfo <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Id queueName
        , responseConsumerTag
        , servedRouteTags
        , responseHandlers
        , publishedTopics
        , subscriberInfo
        }
  release :: T -> IO ()
    -- Closes AMQP consumers, closes connection, and deletes response queue.
    -- Unsubscribes from any subscribed topics.
    -- Not necessary to clear responseHandlers because they automatically time out.
  release t = do
    let T { conn
          , chan
          , responseQueue
          , responseConsumerTag
          , servedRouteTags
          , subscriberInfo
          } = t
        Id _responseQueue = responseQueue
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . snd) servedRouteTags
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo
    _ <- AMQP.deleteQueue chan _responseQueue
    AMQP.closeConnection conn -- Also closes chan.

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
       -- (publish -> res/Stream res -> IO ())
       -- Generic publisher on intermediate result x. Should encapsulate serializing the
       -- intermediate results.
    -> (Text -> Maybe req)
    -> Id "Rpc"
       -- Should throw if Route is already being served.
    -> T
    -> (req -> IO x)
    -> IO ()
    -- Should mutate t to store the details necessary for cleanup.
  _serve publisher deserialize route T {chan, servedRouteTags} handler = do
    HashTable.lookup servedRouteTags route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Id queueName = route
    let handleMsg RequestMessage {id = requestId, responseQueue, reqText} = do
          let Id _responseQueue = responseQueue
              pub response =
                void $
                AMQP.publishMsg
                  chan
                  "" -- Default exchange just sends message to queue specified by routing key.
                  _responseQueue -- Exchange routing key.
                  (toAmqpMsg show ResponseMessage {requestId, response})
          case deserialize reqText of
            Nothing -> pub . Left . BadCall $ "bad input: " <> reqText
            Just r -> handler r >>= publisher (pub . Right)
    _ <- AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) ->
           void . async $ do
             fromAmqpMsg read msg >|>| handleMsg -- Do nothing if request message is malformed.
             AMQP.ackEnv env)
    HashTable.insert servedRouteTags route consumerTag
  _call ::
       IO (res -> IO (), IO x, IO ())
       -- IO (push, result, done)
       -- Generic response handler that takes an intermediate result x and defines how to push it
       -- to the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Id "Rpc"
    -> T
    -> Time Second
       -- Timeout
    -> req
    -> IO x
    -- Registers a handler in the waiting RPC call hash table, and deregisters it after done
    -- resolves.
    -- Timeouts occur if a result or stream result is not received after the specified time.
  _call handler serialize deserializeUnsafe (Id queueName) T { chan
                                                                , responseQueue
                                                                , responseHandlers
                                                                } _timeout req = do
    id <- newUuid
    let request =
          toAmqpMsg
            show
            RequestMessage {id, responseQueue, reqText = serialize req}
    (push, result, waitForDone) <- handler
    renewTimeout <- timeoutThrow' waitForDone _timeout
    HashTable.insert
      responseHandlers
      id
      (\x -> renewTimeout >> deserializeUnsafe x >>= push)
    _ <- AMQP.publishMsg chan "" queueName request
    async $ waitForDone >> HashTable.delete responseHandlers id
    result

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

instance PubSubTransport T where
  _publish :: (a -> Text) -> Id "Topic" -> T -> Streamly.Serial a -> IO ()
  _publish serialize topic T {chan, publishedTopics} as = do
    HashTable.lookup publishedTopics topic >>= \case
      Nothing -> HashTable.insert publishedTopics topic ()
      Just _ -> throw $ AlreadyPublishing topic
    let Id exchangeName = topic
    declarePubSubExchange chan exchangeName
    void . async $
      Streamly.mapM_
        (AMQP.publishMsg chan exchangeName "" . toAmqpMsg serialize) -- Queue name blank.
        as
  _subscribe :: (Text -> IO a) -> Id "Topic" -> T -> IO (Id "Subscriber", Streamly.Serial a)
  -- Returns subscriber ID and result stream as tuple (ID, stream).
  -- Shoud mutate t to store the details necessary for unsubscribe.
  _subscribe deserializeUnsafe (Id exchangeName) T {chan, subscriberInfo} = do
    declarePubSubExchange chan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName exchangeName "" -- Routing key blank.
    (push, close, results) <- repeatableStream
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.NoAck
        (\(msg, _) -> fromAmqpMsg deserializeUnsafe msg >>= push)
    let subscriberId = Id queueName
    HashTable.insert subscriberInfo subscriberId (consumerTag, close)
    return (subscriberId, results)
  unsubscribe :: T -> Id "Subscriber" -> IO ()
  unsubscribe = _unsubscribe
