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
  , headers :: Headers -- Metadata.
  , responseQueue :: Text' "ResponseQueue"
  , reqText :: Text' "Request" -- Should be the serialization of a request object, but this is not
                                -- guaranteed.
  } deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Text' "RequestId"
    -- Other metadata.
  , resText :: Text' "Response"
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
  , responseQueue :: Text' "ResponseQueue"
  , responseConsumerTag :: AMQP.ConsumerTag
  , servedRouteTags :: HashTable (Text' "Rpc") AMQP.ConsumerTag
  , responseHandlers :: HashTable (Text' "RequestId") (Text' "Response" -> IO ())
  , publisherThreads :: HashTable (Text' "TopicName") (Async ())
  , subscriberInfo :: HashTable (Text' "SubscriberId") (AMQP.ConsumerTag, IO ())
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Text' id) <- newUUID
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
    servedRouteTags <- HashTable.new
    publisherThreads <- HashTable.new
    subscriberInfo <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Text' queueName
        , responseConsumerTag
        , servedRouteTags
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
          , servedRouteTags
          , publisherThreads
          , subscriberInfo
          } = t
        Text' _responseQueue = responseQueue
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . snd) servedRouteTags
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo
    _ <- AMQP.deleteQueue chan _responseQueue
    AMQP.closeConnection conn -- Also closes chan.

instance RpcTransport T where
  _serve ::
       ((Text' "Response" -> IO ()) -> Headers -> Text' "Request" -> IO ())
    -> Text' "Rpc"
    -> T
    -> IO ()
  _serve processor route T {chan, servedRouteTags} = do
    HashTable.lookup servedRouteTags route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Text' queueName = route
        handleMsg RequestMessage { id = requestId
                                 , headers
                                 , responseQueue
                                 , reqText
                                 } = do
          let Text' _responseQueue = responseQueue
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
             (read . fromAmqpMsg $ msg) >|>| handleMsg -- Do nothing if request message malformed.
             AMQP.ackEnv env)
    HashTable.insert servedRouteTags route consumerTag
  _call ::
       ((Headers -> Text' "Request" -> IO ()) -> Time Second -> Headers -> req -> IO ( Text' "Response" -> IO ()
                                                                                     , IO x
                                                                                     , IO ()))
    -> Text' "Rpc"
    -> T
    -> Time Second
    -> Headers
    -> req
    -> IO x
  _call processor (Text' queueName) T {chan, responseQueue, responseHandlers} _timeout headers req = do
    id <- newUUID
    let send _headers reqText =
          void $
          AMQP.publishMsg
            chan
            ""
            queueName
            (toAmqpMsg . show $
             RequestMessage {id, headers = _headers, responseQueue, reqText})
    (push, result, waitForDone) <- processor send _timeout headers req
    HashTable.insert responseHandlers id push
    async $ waitForDone >> HashTable.delete responseHandlers id
    result

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

_unsubscribe :: T -> Text' "SubscriberId" -> IO ()
_unsubscribe T {chan, subscriberInfo} subscriberId = do
  let Text' queueName = subscriberId
      maybeUnsubscribe =
        (>|>| \(consumerTag, close) -> do
                AMQP.cancelConsumer chan consumerTag
                AMQP.deleteQueue chan queueName
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
        let Text' exchangeName = topic
        declarePubSubExchange chan exchangeName
        publisherThread <-
          async $
          processor
            (\(Text' a) ->
               void . AMQP.publishMsg chan exchangeName "" . toAmqpMsg $ a -- Queue name is blank.
             )
            as
        HashTable.insert publisherThreads topic publisherThread
      Just _ -> throw $ AlreadyPublishing topic
  _subscribe ::
       IO (Text' "a" -> IO (), IO (), Streamly.Serial a)
    -> Text' "TopicName"
    -> T
    -> IO (Text' "SubscriberId", Streamly.Serial a)
  _subscribe processor (Text' exchangeName) T {chan, subscriberInfo} = do
    (push, close, results) <- processor
    declarePubSubExchange chan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName exchangeName "" -- Routing key blank.
    consumerTag <-
      AMQP.consumeMsgs
        chan
        queueName
        AMQP.NoAck
        (push . Text' . fromAmqpMsg . fst)
    let subscriberId = Text' queueName
    HashTable.insert subscriberInfo subscriberId (consumerTag, close)
    return (subscriberId, results)
  unsubscribe :: T -> Text' "SubscriberId" -> IO ()
  unsubscribe = _unsubscribe
