-- TODO: Different channels for different threads.
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
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: Text' "RequestId"
  , responseQueue :: Text' "ResponseQueue"
  , headers :: Headers
  , reqText :: Text' "Request"
  } deriving (Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Text' "RequestId"
    -- Other metadata.
  , resText :: Text' "Response"
  } deriving (Show, Read)

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

-- Has logic for being both a server and client. Probably not worth separating.
data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Text' "ResponseQueue"
  , responseConsumerTag :: AMQP.ConsumerTag
  , consumedRoutes :: HashTable (Text' "Route") AMQP.ConsumerTag
  , responseHandlers :: HashTable (Text' "RequestId") (Text' "Response" -> IO ())
  , publisherThreads :: HashTable (Text' "TopicName") (Async ())
  , subscriberInfo :: HashTable (Text' "SubscriberId") (AMQP.ConsumerTag, IO ())
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Tagged id) <- newUUID
  myHostName <- Network.HostName.getHostName >>- pack
  return $ myHostName <> "." <> id

fromAmqpMsg :: AMQP.Message -> Text
fromAmqpMsg = pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: Text -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString . unpack $ x}

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
    let handleMsg ResponseMessage {requestId, resText} =
          HashTable.lookup responseHandlers requestId >>= mapM_ ($ resText)
    responseConsumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           (read . fromAmqpMsg $ msg) >|>| handleMsg -- Do nothing if response message malformed.
           AMQP.ackEnv env)
    consumedRoutes <- HashTable.new
    publisherThreads <- HashTable.new
    subscriberInfo <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Tagged queueName
        , responseConsumerTag
        , consumedRoutes
        , responseHandlers
        , publisherThreads
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
          , consumedRoutes
          , publisherThreads
          , subscriberInfo
          } = t
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . snd) consumedRoutes
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo
    _ <- AMQP.deleteQueue chan (untag responseQueue)
    AMQP.closeConnection conn -- Also closes chan.

instance RpcServerTransport T where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> Text' "Route"
    -> T
    -> IO ()
  -- ^ This function SHOULD lock the consumedRoutes table but it's highly unlikely to be a problem
  _consumeRequests asyncHandler route T {chan, consumedRoutes} = do
    HashTable.lookup consumedRoutes route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Tagged queueName = route
    _ <- AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    let asyncHandle RequestMessage {id = requestId, headers, responseQueue, reqText} = do
          let respond resText =
                    void $
                    AMQP.publishMsg
                      chan
                      "" -- Default exchange just sends message to queue specified by routing key.
                      (untag responseQueue) -- Exchange routing key.
                      (toAmqpMsg . show $ ResponseMessage {requestId, resText})
          asyncHandler headers reqText respond
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
             (read . fromAmqpMsg $ msg) >|>| asyncHandle -- Do nothing if request message fails to read.
             AMQP.ackEnv env)
    HashTable.insert consumedRoutes route consumerTag

instance RpcClientTransport T where
  _issueRequest ::
       (Text' "Response" -> IO ())
    -> Text' "Route"
    -> T
    -> Headers
    -> Text' "Request"
    -> IO (IO ())
  _issueRequest respond route T {chan, responseQueue, responseHandlers} headers reqText = do
    id <- newUUID
    AMQP.publishMsg
            chan
            ""
            (untag route)
            (toAmqpMsg . show $
             RequestMessage {id, headers, responseQueue, reqText})
    HashTable.insert responseHandlers id respond
    return (HashTable.delete responseHandlers id)

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

_unsubscribe :: T -> Text' "SubscriberId" -> IO ()
_unsubscribe T {chan, subscriberInfo} subscriberId = do
  let maybeUnsubscribe =
        (>|>| \(consumerTag, close) -> do
                AMQP.cancelConsumer chan consumerTag
                AMQP.deleteQueue chan (untag subscriberId)
                close)
  HashTable.lookup subscriberInfo subscriberId >>= maybeUnsubscribe
  HashTable.delete subscriberInfo subscriberId

instance PubSubTransport T where
  _publish ::
       ((Text' "a" -> IO ()) -> Streamly.Serial a -> IO ())
    -> Text' "TopicName"
    -> Streamly.Serial a
    -> T
    -> IO ()
  _publish processor topic as T {chan, publisherThreads} = do
    HashTable.lookup publisherThreads topic >>= \case
      Nothing -> do
        let Tagged exchangeName = topic
        declarePubSubExchange chan exchangeName
        publisherThread <-
          async $
          processor
            (\(Tagged a) ->
               void . AMQP.publishMsg chan exchangeName "" . toAmqpMsg $ a -- Queue name is blank.
             )
            as
        HashTable.insert publisherThreads topic publisherThread
      Just _ -> throw $ AlreadyPublishing topic
  _subscribe ::
       (Text' "a" -> IO ())
    -> IO ()
    -> Text' "TopicName"
    -> T
    -> IO (Text' "SubscriberId")
  _subscribe push close (Tagged exchangeName) T {chan, subscriberInfo} = do
    declarePubSubExchange chan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName exchangeName "" -- Routing key blank.
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.NoAck
        (push . Tagged . fromAmqpMsg . fst)
    let subscriberId = Tagged queueName
    HashTable.insert subscriberInfo subscriberId (consumerTag, close)
    return subscriberId
  unsubscribe :: T -> Text' "SubscriberId" -> IO ()
  unsubscribe = _unsubscribe
