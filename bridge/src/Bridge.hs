module Bridge
  ( T(..)
  , Config(..)
  , localConfig
  ) where

import Butler
import qualified Control.Concurrent.MVar as MVar
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
  , response :: Either RpcException Text
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
  , serveConsumerTags :: HashTable AMQP.ConsumerTag () -- poor man's mutable set
  , responseHandlers :: HashTable Id (Text -> IO ())
  -- Key: subscriberId to Value: (consumerTag, close)
  , subscriberInfo :: HashTable Id (AMQP.ConsumerTag, IO ())
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

instance Resource T where
  type ResourceConfig T = Config
  -- Connects to bridge, begins listening on RPC queue.
  hold :: Config -> IO T
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
    serveConsumerTags <- HashTable.new
    subscriberInfo <- HashTable.new
    return
      T
        { conn
        , chan
        , responseQueue = Id queueName
        , responseConsumerTag
        , serveConsumerTags
        , responseHandlers
        , subscriberInfo
        }
  release :: T -> IO ()
  release t = do
    let T { conn
          , chan
          , responseQueue
          , responseConsumerTag
          , serveConsumerTags
          , subscriberInfo
          } = t
        Id _responseQueue = responseQueue
    AMQP.cancelConsumer chan responseConsumerTag
    HashTable.mapM_ (AMQP.cancelConsumer chan . fst) serveConsumerTags
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo
    -- responseHandlers will automatically time out.
    _ <- AMQP.deleteQueue chan _responseQueue
    AMQP.closeConnection conn -- And chan.

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ()) -- (publish -> res/Stream res -> IO ())
    -> (Text -> Maybe req)
    -> Route
    -> T
    -> (req -> IO x)
    -> IO () -- should mutate t to store the details necessary for kill
  _serve publisher deserialize (Route queueName) T {chan, serveConsumerTags} handler = do
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
    HashTable.insert serveConsumerTags consumerTag ()
  -- General RPC call.
  -- handler :: IO (responseHandler, result, done) defines how to wrap results up for the caller.
  -- Registers a handler in the waiting RPC call hash table, and deregisters it after done resolves.
  -- Times out if done is not fulfilled after _timeout, with the timeout reset every time a response
  -- is received.
  _call ::
       IO (res -> IO (), IO x, IO ()) -- push, result, done
    -> (req -> Text)
    -> (Text -> IO res)
    -> Route
    -> T
    -> Time Second -- timeout
    -> req
    -> IO x
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

instance PubSubTransport T where
  _publish :: (a -> Text) -> Route -> T -> Streamly.Serial a -> IO ()
  _publish serialize (Route exchangeName) T {chan} as = do
    AMQP.declareExchange
      chan
      AMQP.newExchange {AMQP.exchangeName, AMQP.exchangeType = "fanout"}
    Streamly.mapM_
      (AMQP.publishMsg chan exchangeName "" . toAmqpMsg serialize)
      as -- Queue name blank.
  -- _subscribe should mutate t to store the details necessary for unsubscribe
  _subscribe :: (Text -> IO a) -> Route -> T -> IO (Id, Streamly.Serial a)
  _subscribe deserializeUnsafe (Route route) T {chan, subscriberInfo} = do
    AMQP.declareExchange
      chan
      AMQP.newExchange {AMQP.exchangeName = route, AMQP.exchangeType = "fanout"}
    -- Sanity check to make sure the desired pub/sub route is actually a fanout exchange.
    queueName <- newQueueName
    AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
    AMQP.bindQueue chan queueName route "" -- Routing key blank.
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
