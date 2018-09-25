module Bridge.Transports.Amqp
  ( T(..)
  , Config(..)
  , localConfig
  ) where

import Bridge.Prelude
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
  , headers :: Headers -- Metadata.
  , responseQueue :: Id "ResponseQueue"
  , reqText :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id "RpcRequest"
    -- Other metadata.
  , resText :: Text
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

fromAmqpMsg :: AMQP.Message -> a
fromAmqpMsg = pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: a -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString . unpack . x}

type instance ResourceConfig T = Config

instance Resource T where
  hold :: Config -> IO T
    -- Connects to RabbitMQ, begins listening on RPC queue.
  hold Config {hostname, virtualHost, username, password} = do
    conn <- AMQP.openConnection (unpack hostname) virtualHost username password
    chan <- AMQP.openChannel conn
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    responseHandlers <- HashTable.new
    let handle ResponseMessage {requestId, resText} =
          HashTable.lookup responseHandlers requestId >>= mapM_ ($ resText)
    responseConsumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           (read . fromAmqpMsg $ msg) >|>| handle -- Do nothing if response message is malformed.
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
       ((Text -> IO ()) -> Headers -> Text -> IO ()) -> Id "Rpc" -> t -> IO ()
  _serve processor route T {chan, servedRouteTags} = do
    HashTable.lookup servedRouteTags route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Id queueName = route
        handle RequestMessage {id = requestId, headers, responseQueue, reqText} = do
          let Id _responseQueue = responseQueue
              send resText =
                void $
                AMQP.publishMsg
                  chan
                  "" -- Default exchange just sends message to queue specified by routing key.
                  _responseQueue -- Exchange routing key.
                  (toAmqpMsg . show $ ResponseMessage {requestId, resText})
          processor send headers reqText
    _ <- AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) ->
           void . async $ do
             (read . fromAmqpMsg $ msg) >|>| handle -- Do nothing if request message is malformed.
             AMQP.ackEnv env)
    HashTable.insert servedRouteTags route consumerTag
  _call ::
       ((Text -> IO ()) -> Time Second -> Headers -> req -> IO ( Text -> IO ()
                                                               , IO x
                                                               , IO ()))
    -> Id "Rpc"
    -> t
    -> Time Second
    -> Headers
    -> req
    -> IO x
  _call processor (Id queueName) T {chan, responseQueue, responseHandlers} _timeout headers req = do
    id <- newUuid
    let send reqText =
          AMQP.publishMsg
            chan
            ""
            queueName
            (toAmqpMsg . show $
             RequestMessage {id, headers, responseQueue, reqText})
    (push, result, waitForDone) <- processor send _timeout headers req
    HashTable.insert responseHandlers id push
    async $ waitForDone >> HashTable.delete responseHandlers id
    result

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

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

instance PubSubTransport T where
  _publish ::
       ((Text -> IO ()) -> Streamly.Serial a -> IO ())
    -> Id "Topic"
    -> T
    -> Streamly.Serial a
    -> IO ()
  _publish processor topic T {chan, publishedTopics} as = do
    HashTable.lookup publishedTopics topic >>= \case
      Nothing -> HashTable.insert publishedTopics topic ()
      Just _ -> throw $ AlreadyPublishing topic
    let Id exchangeName = topic
    declarePubSubExchange chan exchangeName
    void . async $
      processor (AMQP.publishMsg chan exchangeName "" . toAmqpMsg) as -- Queue name blank.
  _subscribe ::
       (Text -> IO (), IO (Streamly.Serial a), IO ())
    -- ^ (push subscribe object text, result stream, close)
    -> Id "Topic"
    -- ^ route to listen for wire-messages on
    -> t
    -- ^ transport (should be mutated to store cleanup details)
    -> IO (Id "Subscriber", Streamly.Serial a)
  _subscribe (push, results, close) (Id exchangeName) T {chan, subscriberInfo} = do
    declarePubSubExchange chan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName exchangeName "" -- Routing key blank.
    consumerTag <-
      AMQP.consumeMsgs chan queueName AMQP.NoAck (push . fromAmqpMsg . fst)
    let subscriberId = Id queueName
    HashTable.insert subscriberInfo subscriberId (consumerTag, close)
    return (subscriberId, results)
  unsubscribe :: T -> Id "Subscriber" -> IO ()
  unsubscribe = _unsubscribe
