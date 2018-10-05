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
  , consumedRoutes :: HashTable (NamespacedText' "Route") AMQP.ConsumerTag
  , responseHandlers :: HashTable (Text' "RequestId") (Text' "Response" -> IO ())
  , publisherThreads :: HashTable (NamespacedText' "TopicName") (Async ())
  , subscribers :: HashTable (Text' "SubscriberId") AMQP.ConsumerTag
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
  make :: Config -> IO T
    -- Connects to RabbitMQ, begins listening on RPC queue.
  make Config {hostname, virtualHost, username, password} = do
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
           (read . fromAmqpMsg $ msg) >|>| handleMsg
           -- ^ Do nothing if response message malformed.
           AMQP.ackEnv env)
    consumedRoutes <- HashTable.new
    publisherThreads <- HashTable.new
    subscribers <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Tagged queueName
        , responseConsumerTag
        , consumedRoutes
        , responseHandlers
        , publisherThreads
        , subscribers
        }
  cleanup :: T -> IO ()
    -- Closes AMQP consumers, closes connection, and deletes response queue.
    -- Unsubscribes from any subscribed topics.
  cleanup T { conn
            , chan
            , responseQueue
            , responseConsumerTag
            , consumedRoutes
            , publisherThreads
            , subscribers
            } = do
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . snd) consumedRoutes
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (uncurry (unsubscribe chan)) subscribers
    _ <- AMQP.deleteQueue chan (untag responseQueue)
    AMQP.closeConnection conn -- Also closes chan.

instance RpcTransport T where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> NamespacedText' "Route"
    -> T
    -> IO ()
  -- ^ This function SHOULD lock the consumedRoutes table but it's highly unlikely to be a problem.
  -- TODO: Declare a chan per consumer thread.
  -- (i.e. per call to AMQP.consumeMsgs)
  _consumeRequests asyncHandler route T {chan, consumedRoutes} = do
    HashTable.lookup consumedRoutes route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Tagged queueName = route
    _ <- AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    let asyncHandle RequestMessage { id = requestId
                                   , headers
                                   , responseQueue
                                   , reqText
                                   } = do
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
           (read . fromAmqpMsg $ msg) >|>| asyncHandle -- Do nothing if message fails to read.
           AMQP.ackEnv env)
    HashTable.insert consumedRoutes route consumerTag
  _issueRequest ::
       (Text' "Response" -> IO ())
    -> NamespacedText' "Route"
    -> T
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())
  _issueRequest respond route T {chan, responseQueue, responseHandlers} headers reqText = do
    id <- newUUID
    HashTable.insert responseHandlers id respond
    AMQP.publishMsg
      chan
      ""
      (untag route)
      (toAmqpMsg . show $ RequestMessage {id, headers, responseQueue, reqText})
    return $ Tagged $ HashTable.delete responseHandlers id

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

unsubscribe :: AMQP.Channel -> Text' "SubscriberId" -> AMQP.ConsumerTag -> IO ()
unsubscribe chan subscriberId consumerTag = do
  AMQP.cancelConsumer chan consumerTag
  AMQP.deleteQueue chan (untag subscriberId) & void

instance PubSubTransport T where
  _publish ::
       ((Text' "a" -> IO ()) -> IO ())
    -> NamespacedText' "TopicName"
    -> T
    -> IO ()
  -- Like _serve, has race condition on publisherThreads. Not likely to be a problem
  _publish publisher topic T {chan, publisherThreads} = do
    HashTable.lookup publisherThreads topic >>= \case
      Just _ -> throw $ AlreadyPublishing topic
      Nothing -> return ()
    let Tagged exchangeName = topic
    declarePubSubExchange chan exchangeName
    let send (Tagged a) =
          void . AMQP.publishMsg chan exchangeName "" . toAmqpMsg $ a -- Queue name is blank.
    publisherThread <- async $ publisher send
    HashTable.insert publisherThreads topic publisherThread
  _subscribe ::
       (Text' "a" -> IO ())
    -> NamespacedText' "TopicName"
    -> T
    -> IO (IO' "Unsubscribe" ())
  -- TODO: Declare a chan per consumer thread.
  -- (i.e. per call to AMQP.consumeMsgs)
  _subscribe push (Tagged exchangeName) T {chan, subscribers} = do
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
    HashTable.insert subscribers subscriberId consumerTag
    let unsubscribe_ = do
          HashTable.delete subscribers subscriberId
          unsubscribe chan subscriberId consumerTag
    return $ Tagged unsubscribe_
