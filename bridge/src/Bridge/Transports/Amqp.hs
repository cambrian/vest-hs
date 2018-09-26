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
  { id :: Id "RpcRequest"
  , headers :: Headers -- Metadata.
  , responseQueue :: Id "ResponseQueue"
  , reqText :: Id "RequestText" -- Should be the serialization of a request object, but this is not
                                -- guaranteed.
  } deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id "RpcRequest"
    -- Other metadata.
  , resText :: Id "ResponseText"
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
  , responseHandlers :: HashTable (Id "RpcRequest") (Id "ResponseText" -> IO ())
  , publisherThreads :: HashTable (Id "Topic") (Async ())
  , subscriberInfo :: HashTable (Id "Subscriber") (AMQP.ConsumerTag, IO ())
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Id id) <- newUuid
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
        , responseQueue = Id queueName
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
        Id _responseQueue = responseQueue
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . snd) servedRouteTags
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo
    _ <- AMQP.deleteQueue chan _responseQueue
    AMQP.closeConnection conn -- Also closes chan.

instance RpcTransport T where
  _serve ::
       ((Id "ResponseText" -> IO ()) -> Headers -> Id "RequestText" -> IO ())
    -> Id "Rpc"
    -> T
    -> IO ()
  _serve processor route T {chan, servedRouteTags} = do
    HashTable.lookup servedRouteTags route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServing route
    let Id queueName = route
        handleMsg RequestMessage { id = requestId
                                 , headers
                                 , responseQueue
                                 , reqText
                                 } = do
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
             (read . fromAmqpMsg $ msg) >|>| handleMsg -- Do nothing if request message malformed.
             AMQP.ackEnv env)
    HashTable.insert servedRouteTags route consumerTag
  _call ::
       ((Headers -> Id "RequestText" -> IO ()) -> Time Second -> Headers -> req -> IO ( Id "ResponseText" -> IO ()
                                                                                      , IO x
                                                                                      , IO ()))
    -> Id "Rpc"
    -> T
    -> Time Second
    -> Headers
    -> req
    -> IO x
  _call processor (Id queueName) T {chan, responseQueue, responseHandlers} _timeout headers req = do
    id <- newUuid
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
       ((Id "PublishText" -> IO ()) -> Streamly.Serial a -> IO ())
    -> Id "Topic"
    -> Streamly.Serial a
    -> T
    -> IO ()
  _publish processor topic as T {chan, publisherThreads} = do
    HashTable.lookup publisherThreads topic >>= \case
      Nothing -> do
        let Id exchangeName = topic
        declarePubSubExchange chan exchangeName
        publisherThread <-
          async $
          processor
            (\(Id a) ->
               void . AMQP.publishMsg chan exchangeName "" . toAmqpMsg $ a -- Queue name is blank.
             )
            as
        HashTable.insert publisherThreads topic publisherThread
      Just _ -> throw $ AlreadyPublishing topic
  _subscribe ::
       IO (Id "PublishText" -> IO (), IO (), Streamly.Serial a)
    -> Id "Topic"
    -> T
    -> IO (Id "Subscriber", Streamly.Serial a)
  _subscribe processor (Id exchangeName) T {chan, subscriberInfo} = do
    (push, close, results) <- processor
    declarePubSubExchange chan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName exchangeName "" -- Routing key blank.
    consumerTag <-
      AMQP.consumeMsgs chan queueName AMQP.NoAck (push . Id . fromAmqpMsg . fst)
    let subscriberId = Id queueName
    HashTable.insert subscriberInfo subscriberId (consumerTag, close)
    return (subscriberId, results)
  unsubscribe :: T -> Id "Subscriber" -> IO ()
  unsubscribe = _unsubscribe
