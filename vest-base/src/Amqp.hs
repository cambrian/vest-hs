module Amqp
  ( T(..)
  , Config(..)
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.Map as Map
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.Types as AMQP.Types
import qualified TMap
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
  , servedRoutes :: TMap Route (AMQP.Channel, AMQP.ConsumerTag)
  , responseHandlers :: TMap (UUID' "Request") (Text' "Response" -> IO ())
  , subscribers :: TMap (Text' "SubscriberId") (AMQP.Channel, AMQP.ConsumerTag)
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
    responseHandlers <- TMap.newIO
    responseConsumerTag <-
      AMQP.consumeMsgs
        responseConsumerChan
        queueName
        AMQP.Ack
        (\(msg, env) -> do
           void . runMaybeT $ do
             ResponseMessage {requestId, resText} <-
               MaybeT $ return $ read $ fromAmqpMsg msg
             handler <-
               MaybeT $ atomically $ TMap.lookup requestId responseHandlers
             liftIO $ handler resText
           AMQP.ackEnv env)
    servedRoutes <- TMap.newIO
    subscribers <- TMap.newIO
    return
      T
        { conn
        , publishChan
        , responseQueue = Tagged queueName
        , responseConsumerChan
        , responseConsumerTag
        , servedRoutes
        , responseHandlers
        , subscribers
        }
  cleanup :: T -> IO ()
    -- Closes AMQP consumers, closes connection, and deletes response queue.
    -- Unsubscribes from any subscribed topics.
  cleanup T { conn
            , responseQueue
            , responseConsumerChan
            , responseConsumerTag
            , servedRoutes
            , subscribers
            } = do
    AMQP.cancelConsumer responseConsumerChan responseConsumerTag
    TMap.parallelMapValuesM_ (uncurry AMQP.cancelConsumer) servedRoutes
    TMap.parallelMapM_ (uncurry cancelConsumerDeleteQueue) subscribers
    void $ AMQP.deleteQueue responseConsumerChan (untag responseQueue)
    AMQP.closeConnection conn -- Also closes chans.

instance RpcTransport T where
  serveRaw ::
       T
    -> Route
    -> (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> IO ()
  -- ^ This function SHOULD lock the servedRoutes table but it's highly unlikely to be a problem.
  serveRaw T {conn, publishChan, servedRoutes} route asyncHandler = do
    whenM (atomically $ isJust <$> TMap.lookup route servedRoutes) $
      throw $ AlreadyServingException route
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
    atomically $ TMap.insert (consumerChan, consumerTag) route servedRoutes
  callRaw ::
       T
    -> Route
    -> Headers
    -> Text' "Request"
    -> (IO (Text' "Response") -> IO a)
    -> IO a
  callRaw T {publishChan, responseQueue, responseHandlers} route headers reqText f = do
    id <- nextUUID'
    q <- newTQueueIO
    atomically $ TMap.insert (atomically . writeTQueue q) id responseHandlers
    void $
      AMQP.publishMsg
        publishChan
        ""
        (untag route)
        (toAmqpMsg . show $ RequestMessage {id, headers, responseQueue, reqText})
    finally
      (f $ atomically $ readTQueue q)
      (atomically $ TMap.delete id responseHandlers)

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

cancelConsumerDeleteQueue ::
     Text' t -> (AMQP.Channel, AMQP.ConsumerTag) -> IO ()
-- ^ Also cleans up queue
cancelConsumerDeleteQueue (Tagged queue) (consumerChan, consumerTag) = do
  AMQP.cancelConsumer consumerChan consumerTag
  void $ AMQP.deleteQueue consumerChan queue

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
  atomically $ TMap.insert (consumerChan, consumerTag) subscriberId subscribers

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

instance Loadable T where
  configFile = [relfile|amqp.yaml|]
