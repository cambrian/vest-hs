module Transport.Amqp
  ( T(..)
  , Config(..)
  , localConfig
  ) where

import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import Vest

type HashTable k v = HashTable.BasicHashTable k v

data RequestMessage = RequestMessage
  { id :: UUID' "Request"
  , responseQueue :: Text' "ResponseQueue"
  , headers :: Headers
  , reqText :: Text' "Request"
  } deriving (Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: UUID' "Request"
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
  , publishChan :: AMQP.Channel
  , responseQueue :: Text' "ResponseQueue"
  , responseConsumerChan :: AMQP.Channel
  , responseConsumerTag :: AMQP.ConsumerTag
  , consumedRoutes :: HashTable (NamespacedText' "Route") ( AMQP.Channel
                                                          , AMQP.ConsumerTag)
  , responseHandlers :: HashTable (UUID' "Request") (Text' "Response" -> IO ())
  , publisherThreads :: HashTable (NamespacedText' "TopicName") (Async ())
  , subscribers :: HashTable (Text' "SubscriberId") ( AMQP.Channel
                                                    , AMQP.ConsumerTag)
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Tagged id) <- nextUUID'
  myHostName <- Network.HostName.getHostName >>- pack
  return $ myHostName <> "." <> show id

fromAmqpMsg :: AMQP.Message -> Text
fromAmqpMsg = pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: Text -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString . unpack $ x}

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
    -- Connects to RabbitMQ, begins listening on RPC queue.
  make Config {hostname, virtualHost, username, password} --
   = do
    conn <- AMQP.openConnection (unpack hostname) virtualHost username password
    publishChan <- AMQP.openChannel conn
    responseConsumerChan <- AMQP.openChannel conn
    queueName <- newQueueName
    AMQP.declareQueue responseConsumerChan AMQP.newQueue {AMQP.queueName}
    responseHandlers <- HashTable.new
    let handleMsg ResponseMessage {requestId, resText} =
          HashTable.lookup responseHandlers requestId >>= mapM_ ($ resText)
    responseConsumerTag <-
      AMQP.consumeMsgs
        responseConsumerChan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           forM_ (read $ fromAmqpMsg msg) handleMsg
           -- ^ Do nothing if response message malformed.
           AMQP.ackEnv env)
    consumedRoutes <- HashTable.new
    publisherThreads <- HashTable.new
    subscribers <- HashTable.new
    return
      T
        { conn
        , publishChan
        , responseQueue = Tagged queueName
        , responseConsumerChan
        , responseConsumerTag
        , consumedRoutes
        , responseHandlers
        , publisherThreads
        , subscribers
        }
  cleanup :: T -> IO ()
    -- Closes AMQP consumers, closes connection, and deletes response queue.
    -- Unsubscribes from any subscribed topics.
  cleanup T { conn = _
            , responseQueue
            , responseConsumerChan
            , responseConsumerTag
            , consumedRoutes
            , publisherThreads
            , subscribers
            } = do
    AMQP.cancelConsumer responseConsumerChan responseConsumerTag
    HashTable.mapM_ (uncurry AMQP.cancelConsumer . snd) consumedRoutes
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (uncurry unsubscribe) subscribers
    _ <- AMQP.deleteQueue responseConsumerChan (untag responseQueue)
    -- AMQP.closeConnection is not thread safe, so we choose to leak the connection instead.
    -- AMQP.closeConnection conn -- Also closes chans.
    -- We have to manually close the AMQP channels now
    AMQP.closeChannel responseConsumerChan
    HashTable.mapM_ (AMQP.closeChannel . fst . snd) consumedRoutes
    HashTable.mapM_ (AMQP.closeChannel . fst . snd) subscribers

instance RpcTransport T where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> NamespacedText' "Route"
    -> T
    -> IO ()
  -- ^ This function SHOULD lock the consumedRoutes table but it's highly unlikely to be a problem.
  _consumeRequests asyncHandler route T {conn, publishChan, consumedRoutes} = do
    HashTable.lookup consumedRoutes route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServingException route
    let Tagged queueName = route
    consumerChan <- AMQP.openChannel conn
    _ <- AMQP.declareQueue consumerChan AMQP.newQueue {AMQP.queueName}
    let asyncHandle RequestMessage { id = requestId
                                   , headers
                                   , responseQueue
                                   , reqText
                                   } = do
          let respond resText =
                void $
                AMQP.publishMsg
                  publishChan
                  "" -- Default exchange just sends message to queue specified by routing key.
                  (untag responseQueue) -- Exchange routing key.
                  (toAmqpMsg . show $ ResponseMessage {requestId, resText})
          asyncHandler headers reqText respond
    consumerTag <-
      AMQP.consumeMsgs
        consumerChan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           forM_ (read $ fromAmqpMsg msg) asyncHandle -- Do nothing if message fails to read.
           AMQP.ackEnv env)
    HashTable.insert consumedRoutes route (consumerChan, consumerTag)
  _issueRequest ::
       (Text' "Response" -> IO ())
    -> NamespacedText' "Route"
    -> T
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())
  _issueRequest respond route T {publishChan, responseQueue, responseHandlers} headers reqText = do
    id <- nextUUID'
    HashTable.insert responseHandlers id respond
    AMQP.publishMsg
      publishChan
      ""
      (untag route)
      (toAmqpMsg . show $ RequestMessage {id, headers, responseQueue, reqText})
    return $ Tagged $ HashTable.delete responseHandlers id

declarePubSubExchange :: AMQP.Channel -> Text -> IO ()
declarePubSubExchange chan exchangeName =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

unsubscribe :: Text' "SubscriberId" -> (AMQP.Channel, AMQP.ConsumerTag) -> IO ()
unsubscribe subscriberId (consumerChan, consumerTag) = do
  AMQP.cancelConsumer consumerChan consumerTag
  AMQP.deleteQueue consumerChan (untag subscriberId) & void

instance PubSubTransport T where
  _publish ::
       ((Text' "a" -> IO ()) -> IO ())
    -> NamespacedText' "TopicName"
    -> T
    -> IO ()
  -- Like _serve, has race condition on publisherThreads. Not likely to be a problem.
  _publish publisher topic T {publishChan, publisherThreads} = do
    HashTable.lookup publisherThreads topic >>= \case
      Just _ -> throw $ AlreadyPublishing topic
      Nothing -> return ()
    let Tagged exchangeName = topic
    declarePubSubExchange publishChan exchangeName
    let send (Tagged a) =
          void . AMQP.publishMsg publishChan exchangeName "" . toAmqpMsg $ a -- Queue name is blank.
    publisherThread <- async $ publisher send
    HashTable.insert publisherThreads topic publisherThread
  _subscribe ::
       (Text' "a" -> IO ())
    -> NamespacedText' "TopicName"
    -> T
    -> IO (IO' "Unsubscribe" ())
  _subscribe push (Tagged exchangeName) T {conn, subscribers} = do
    consumerChan <- AMQP.openChannel conn
    declarePubSubExchange consumerChan exchangeName
    queueName <- newQueueName
    AMQP.declareQueue consumerChan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue consumerChan queueName exchangeName "" -- Routing key blank.
    consumerTag <-
      AMQP.consumeMsgs
        consumerChan
        queueName
        AMQP.NoAck
        (push . Tagged . fromAmqpMsg . fst)
    let subscriberId = Tagged queueName
    HashTable.insert subscribers subscriberId (consumerChan, consumerTag)
    let unsubscribe_ = do
          HashTable.delete subscribers subscriberId
          unsubscribe subscriberId (consumerChan, consumerTag)
    return $ Tagged unsubscribe_

instance HasRpcTransport T T where
  rpcTransport = identity

instance HasPubSubTransport T T where
  pubSubTransport = identity
