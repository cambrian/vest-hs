module Transport.Amqp
  ( T(..)
  , Config(..)
  ) where

import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Map as Map
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.Types as AMQP.Types
import Vest

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
  } deriving (Generic, FromJSON)

-- Has logic for being both a server and client. Probably not worth separating.
data T = T
  { conn :: AMQP.Connection
  , publishChan :: AMQP.Channel
  , responseQueue :: Text' "ResponseQueue"
  , responseConsumerChan :: AMQP.Channel
  , responseConsumerTag :: AMQP.ConsumerTag
  , consumedRoutes :: HashTable Route (AMQP.Channel, AMQP.ConsumerTag)
  , responseHandlers :: HashTable (UUID' "Request") (Text' "Response" -> IO ())
  , subscribers :: HashTable (Text' "SubscriberId") ( AMQP.Channel
                                                    , AMQP.ConsumerTag)
    -- ^ Key: subscriberId to Value: (consumerTag, close)
  }

newQueueName :: IO Text
newQueueName = do
  (Tagged id) <- nextUUID'
  hostpid <- getHostPid
  return $ hostpid <> "." <> show id

fromAmqpMsg :: AMQP.Message -> Text
fromAmqpMsg = pack . ByteString.Lazy.UTF8.toString . AMQP.msgBody

toAmqpMsg :: Text -> AMQP.Message
toAmqpMsg x =
  AMQP.newMsg {AMQP.msgBody = ByteString.Lazy.UTF8.fromString . unpack $ x}

instance Resource T where
  type ResourceConfig T = Config
  make :: Config -> IO T
    -- Connects to RabbitMQ, begins listening on RPC queue.
  make Config {hostname, virtualHost, username, password} = do
    conn <- AMQP.openConnection (unpack hostname) virtualHost username password
    publishChan <- AMQP.openChannel conn
    responseConsumerChan <- AMQP.openChannel conn
    queueName <- newQueueName
    AMQP.declareQueue responseConsumerChan AMQP.newQueue {AMQP.queueName}
    responseHandlers <- HashTable.new
    responseConsumerTag <-
      AMQP.consumeMsgs
        responseConsumerChan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           void . runMaybeT $ do
             ResponseMessage {requestId, resText} <-
               MaybeT $ return $ read $ fromAmqpMsg msg
             handler <- MaybeT $ HashTable.lookup responseHandlers requestId
             liftIO $ handler resText
           AMQP.ackEnv env)
    consumedRoutes <- HashTable.new
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
            , subscribers
            } = do
    AMQP.cancelConsumer responseConsumerChan responseConsumerTag
    HashTable.mapM_ (uncurry AMQP.cancelConsumer . snd) consumedRoutes
    HashTable.mapM_ (uncurry unsubscribe) subscribers
    void $ AMQP.deleteQueue responseConsumerChan (untag responseQueue)
    -- AMQP.closeConnection is not thread safe, so we choose to leak the connection instead.
    -- AMQP.closeConnection conn -- Also closes chans.
    -- We have to manually close the AMQP channels now
    AMQP.closeChannel responseConsumerChan
    HashTable.mapM_ (AMQP.closeChannel . fst . snd) consumedRoutes
    HashTable.mapM_ (AMQP.closeChannel . fst . snd) subscribers

instance RpcTransport T where
  serveRaw ::
       T
    -> Route
    -> (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> IO ()
  -- ^ This function SHOULD lock the consumedRoutes table but it's highly unlikely to be a problem.
  serveRaw T {conn, publishChan, consumedRoutes} route asyncHandler = do
    HashTable.lookup consumedRoutes route >>= \case
      Nothing -> return ()
      Just _ -> throw $ AlreadyServingException route
    let Tagged queueName = route
    consumerChan <- AMQP.openChannel conn
    void $ AMQP.declareQueue consumerChan AMQP.newQueue {AMQP.queueName}
    consumerTag <-
      AMQP.consumeMsgs
        consumerChan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           void . runMaybeT $ do
             RequestMessage {id = requestId, headers, responseQueue, reqText} <-
               MaybeT $ return $ read $ fromAmqpMsg msg
             let respond resText =
                   void $
                   AMQP.publishMsg
                     publishChan
                     "" -- Default exchange just sends message to queue specified by routing key.
                     (untag responseQueue) -- Exchange routing key.
                     (toAmqpMsg . show $ ResponseMessage {requestId, resText})
             liftIO $ asyncHandler headers reqText respond
           AMQP.ackEnv env)
    HashTable.insert consumedRoutes route (consumerChan, consumerTag)
  callRaw ::
       T
    -> Route
    -> Headers
    -> Text' "Request"
    -> (Text' "Response" -> IO ())
    -> IO (IO' "Cleanup" ())
  callRaw T {publishChan, responseQueue, responseHandlers} route headers reqText respond = do
    id <- nextUUID'
    HashTable.insert responseHandlers id respond
    AMQP.publishMsg
      publishChan
      ""
      (untag route)
      (toAmqpMsg . show $ RequestMessage {id, headers, responseQueue, reqText})
    return $ Tagged $ HashTable.delete responseHandlers id

declareValueExchange :: AMQP.Channel -> ValueName -> IO ()
declareValueExchange chan (Tagged exchangeName) =
  AMQP.declareExchange
    chan
    AMQP.newExchange
      { AMQP.exchangeName
      , AMQP.exchangeType = "x-recent-history"
      , AMQP.exchangeArguments =
          AMQP.Types.FieldTable $
          Map.fromList [("x-recent-history-length", AMQP.Types.FVInt32 1)]
      }

declareEventExchange :: AMQP.Channel -> EventName -> IO ()
declareEventExchange chan (Tagged exchangeName) =
  AMQP.declareExchange
    chan
    AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}

unsubscribe :: Text' "SubscriberId" -> (AMQP.Channel, AMQP.ConsumerTag) -> IO ()
unsubscribe subscriberId (consumerChan, consumerTag) = do
  AMQP.cancelConsumer consumerChan consumerTag
  AMQP.deleteQueue consumerChan (untag subscriberId) & void

publish_ :: T -> Text' t -> Text' "a" -> IO ()
publish_ T {publishChan} (Tagged exchangeName) =
  void . AMQP.publishMsg publishChan exchangeName "" . toAmqpMsg . untag

subscribe_ :: T -> Text' t -> (Text' "a" -> IO ()) -> IO ()
subscribe_ T {conn, subscribers} (Tagged exchangeName) push = do
  consumerChan <- AMQP.openChannel conn
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

instance ValueTransport T where
  publishValue t valueName = do
    declareValueExchange (publishChan t) valueName
    return $ publish_ t valueName
  subscribeValue t valueName f = do
    declareValueExchange (publishChan t) valueName
    subscribe_ t valueName f

instance EventTransport T where
  publishEvents t eventName = do
    declareEventExchange (publishChan t) eventName
    return $ publish_ t eventName
  subscribeEvents t eventName f = do
    declareEventExchange (publishChan t) eventName
    subscribe_ t eventName f

instance HasRpcTransport T T where
  rpcTransport = identity

instance HasValueTransport T T where
  valueTransport = identity

instance HasEventTransport T T where
  eventTransport = identity

instance Loadable T where
  configFile = [relfile|amqp.yaml|]
