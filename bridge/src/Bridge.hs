module Bridge
  ( Config(..)
  , T(..)
  , defaultTimeout
  , make
  , serveRPC
  , serveRPC'
  , callRPC
  , callRPC'
  , callRPCTimeout
  , callRPCTimeout'
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import qualified Data.HashTable.IO as HashTable
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.AMQP as AMQP
import qualified Network.HostName
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

defaultTimeout :: DiffTime
defaultTimeout = secondsToDiffTime 10

data BridgeException
  = BadCall Text
  | InternalBridgeException Text -- If you get one of these, file a bug report.
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception BridgeException

data RequestMessage = RequestMessage
  { id :: Id
  , responseQueue :: Id
    -- Other metadata (time?).
  , req :: Text -- Should be the serialization of a request object, but this is not guaranteed.
  } deriving (Eq, Show, Read)

data Response
  = Result Text -- Should be the serialization of a result object, but this is not guaranteed.
  | EndOfResults
  deriving (Eq, Show, Read)

data ResponseMessage = ResponseMessage
  { requestId :: Id
    -- other metadata
  , res :: Either BridgeException Response
  } deriving (Eq, Show, Read)

data Config = Config
  { hostname :: Text
  , virtualHost :: Text
  , username :: Text
  , password :: Text
  } deriving (Eq, Show, Read)

data T = T
  { conn :: AMQP.Connection
  , chan :: AMQP.Channel
  , responseQueue :: Id
  , waitingCalls :: HashTable Id (Response -> IO ())
  }

-- Connects to bridge, begins listening on RPC response queue.
-- Maintains a hash table of open RPC calls.
-- For each incoming message on the response queue, make
-- will attempt to dispatch it to the corresponding waiting RPC call.
make :: Config -> IO T
make Config {hostname, virtualHost, username, password} = do
  conn <-
    AMQP.openConnection (Text.unpack hostname) virtualHost username password
  chan <- AMQP.openChannel conn
  myHostname <- Network.HostName.getHostName
  uuid <- UUID.nextRandom
  let queueName = Text.pack (myHostname ++ UUID.toString uuid)
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  waitingCalls <- HashTable.new
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       let rawMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
       case readMaybe rawMsg of
         Nothing -> return ()
         Just ResponseMessage {requestId, res} ->
           case res of
             Left exception -> throwIO exception
             Right r -> HashTable.lookup waitingCalls requestId >>= mapM_ ($ r)
       AMQP.ackEnv env)
  return T {conn, chan, responseQueue = Id queueName, waitingCalls}

-- General RPC server implementation. publisher :: publish -> res -> IO () defines
-- how results should be communicated back to the client.
_serveRPC ::
     (Read req)
  => ((Response -> IO ()) -> res -> IO ())
  -> T
  -> Route
  -> (req -> IO res)
  -> IO ()
_serveRPC publisher T {chan} route handler = do
  let Route queueName = route
  AMQP.declareQueue chan AMQP.newQueue {AMQP.queueName}
  AMQP.consumeMsgs
    chan
    queueName
    AMQP.Ack
    (\(msg, env) -> do
       forkIO $ do
         let rawReqMsg = msg & AMQP.msgBody & ByteString.Lazy.UTF8.toString
         case readMaybe rawReqMsg of
           Nothing -> return ()
           Just RequestMessage {id = requestId, responseQueue, req} -> do
             let Id _responseQueue = responseQueue
             let publish res = do
                   let msgBody =
                         ByteString.Lazy.UTF8.fromString $
                         show $ ResponseMessage {requestId, res}
                   AMQP.publishMsg
                     chan
                     "" -- Default exchange just sends message to queue specified by routing key.
                     _responseQueue -- Exchange routing key.
                     AMQP.newMsg {AMQP.msgBody}
                   return ()
             case readMaybe $ Text.unpack req of
               Nothing ->
                 publish . Left . BadCall $ Text.append "bad input: " req
               Just r -> handler r >>= publisher (publish . Right)
         AMQP.ackEnv env
       return ())
  return ()

-- Direct RPC server
serveRPC :: (Read req, Show res) => T -> Route -> (req -> IO res) -> IO ()
serveRPC = _serveRPC (\publish res -> publish $ Result (show res))

-- Streaming RPC server
serveRPC' ::
     (Read req, Show res)
  => T
  -> Route
  -> (req -> IO (Streamly.Serial res))
  -> IO ()
serveRPC' =
  _serveRPC
    (\publish results -> do
       Streamly.mapM_ (publish . Result . show) results
       publish EndOfResults)

-- General RPC call.
-- handler :: IO (responseHandler, result, done) defines how to wrap results
-- up for the caller. Registers a handler in the waiting RPC call hash table,
-- and deregisters it after done resolves. Times out if done is not fulfilled
-- after _timeout, with the timeout reset every time a response is received.
_callRPCTimeout ::
     (Show req)
  => IO (Response -> IO (), IO res, IO ())
  -> DiffTime
  -> T
  -> Route
  -> req
  -> IO res
_callRPCTimeout handler _timeout T {chan, responseQueue, waitingCalls} route req = do
  let Route queueName = route
  id <- fmap (Id . UUID.toText) UUID.nextRandom
  let reqMsg = RequestMessage {id, responseQueue, req = show req}
  let msgBody = ByteString.Lazy.UTF8.fromString $ show reqMsg
  (h, res, done) <- handler
  renewTimeout <- timeoutThrowIO' done _timeout
  HashTable.insert waitingCalls id (\x -> renewTimeout >> h x)
  forkIO $ done >> HashTable.delete waitingCalls id
  _ <- AMQP.publishMsg chan "" queueName AMQP.newMsg {AMQP.msgBody}
  res

-- Direct RPC call
callRPCTimeout ::
     forall req res. (Show req, Read res)
  => DiffTime
  -> T
  -> Route
  -> req
  -> IO res
callRPCTimeout =
  _callRPCTimeout
    (do result <- MVar.newEmptyMVar
        let handler =
              \case
                Result res -> MVar.putMVar result (read @res (Text.unpack res))
                EndOfResults ->
                  throwIO $
                  InternalBridgeException
                    "Direct RPC should never get EndOfResults"
        let res = MVar.readMVar result
        return (handler, res, void res))

-- Streaming RPC call
callRPCTimeout' ::
     forall req res. (Show req, Read res)
  => DiffTime
  -> T
  -> Route
  -> req
  -> IO (Streamly.Serial res)
callRPCTimeout' =
  _callRPCTimeout
    (do (push, results) <- repeatableStream
        let handler =
              \case
                Result res -> push (Just (read @res (Text.unpack res)))
                EndOfResults -> push Nothing
        let done = Streamly.mapM_ return results
        return (handler, return results, done))

-- Direct RPC call with default timeout
callRPC :: (Show req, Read res) => T -> Route -> req -> IO res
callRPC = callRPCTimeout defaultTimeout

-- Streaming RPC call with default timeout
callRPC' ::
     (Show req, Read res) => T -> Route -> req -> IO (Streamly.Serial res)
callRPC' = callRPCTimeout' defaultTimeout
