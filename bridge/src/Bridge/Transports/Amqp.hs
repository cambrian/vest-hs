module Bridge.Transports.Amqp
  ( T(..)
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
  { id :: Id
  , responseQueue :: Id
    -- Other metadata (time?).
  , reqText :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
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
  , responseQueue :: Id
  , responseConsumerTag :: AMQP.ConsumerTag
  , servedRouteTags :: HashTable Route AMQP.ConsumerTag
  , responseHandlers :: HashTable Id (Text -> IO ())
  , subscriberInfo :: HashTable Id (AMQP.ConsumerTag, IO ())
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

_unsubscribe :: T -> Id -> IO ()
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
    subscriberInfo <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Id queueName
        , responseConsumerTag
        , servedRouteTags
        , responseHandlers
        , subscriberInfo
        }
  release :: T -> IO ()
    -- Closes AMQP consumers, connection, deletes response queue.
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
    -> Route
       -- Should throw if Route is already being served.
    -> T
    -> (req -> IO x)
    -> IO ()
    -- Should mutate t to store the details necessary for cleanup.
  _serve publisher deserialize (Route queueName) T {chan, servedRouteTags} handler = do
    HashTable.lookup servedRouteTags (Route queueName) >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing (Route queueName)
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
    HashTable.insert servedRouteTags (Route queueName) consumerTag
  _call ::
       IO (res -> IO (), IO x, IO ())
       -- IO (push, result, done)
       -- Generic response handler that takes an intermediate result x and defines how to push it
       -- to the caller.
    -> (req -> Text)
    -> (Text -> IO res)
    -> Route
    -> T
    -> Time Second
       -- Timeout
    -> req
    -> IO x
    -- Registers a handler in the waiting RPC call hash table, and deregisters it after done
    -- resolves.
    -- Timeouts occur if a result or stream result is not received after the specified time.
  _call handler serialize deserializeUnsafe (Route queueName) T { chan
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
  _publish :: (a -> Text) -> Route -> T -> Streamly.Serial a -> IO ()
  _publish serialize (Route exchangeName) T {chan} as = do
    declarePubSubExchange chan exchangeName
    void . async $
      Streamly.mapM_
        (AMQP.publishMsg chan exchangeName "" . toAmqpMsg serialize) -- Queue name blank.
        as
  _subscribe :: (Text -> IO a) -> Route -> T -> IO (Id, Streamly.Serial a)
  -- Returns subscriber ID and result stream as tuple (ID, stream).
  -- Shoud mutate t to store the details necessary for unsubscribe.
  _subscribe deserializeUnsafe (Route exchangeName) T {chan, subscriberInfo} = do
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
  unsubscribe :: T -> Id -> IO ()
  unsubscribe = _unsubscribe
