-- Refer to AMQP and WebSocket transports for detailed commentary.
-- This transport is ONLY to be used for testing purposes!
module Bridge.Transports.Dummy
  ( T(..)
  , Config
  , localConfig
  ) where

import Bridge.PubSub
import Bridge.Rpc
import qualified Data.HashTable.IO as HashTable
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type HashTable k v = HashTable.BasicHashTable k v

type Config = ()

localConfig :: Config
localConfig = ()

data T = T
  { requestVars :: HashTable (Id "Rpc") (MVar (Id "RpcRequest", Text)) -- Route to request.
  , responseVars :: HashTable (Id "RpcRequest") (MVar Text) -- Call ID to response.
  , serverThreads :: HashTable (Id "Rpc") (Async ()) -- List of serving threads.
  , publisherThreads :: HashTable (Id "Topic") (Async ()) -- List of publisher threads.
  , streams :: HashTable (Id "Topic") (Text -> IO (), Streamly.Serial Text) -- PubSub streams.
  , subscriberInfo :: HashTable (Id "Subscriber") (Async (), IO ()) -- ID to (thread, close).
  }

_unsubscribe :: T -> Id "Subscriber" -> IO ()
_unsubscribe T {subscriberInfo} subscriberId = do
  let maybeUnsubscribe =
        (>|>| \(thread, close) -> do
                cancel thread
                close)
  HashTable.lookup subscriberInfo subscriberId >>= maybeUnsubscribe
  HashTable.delete subscriberInfo subscriberId

type instance ResourceConfig T = Config

instance Resource T where
  hold :: Config -> IO T
  hold _ = do
    requestVars <- HashTable.new
    responseVars <- HashTable.new
    serverThreads <- HashTable.new
    publisherThreads <- HashTable.new
    streams <- HashTable.new
    subscriberInfo <- HashTable.new
    return
      T
        { requestVars
        , responseVars
        , serverThreads
        , publisherThreads
        , streams
        , subscriberInfo
        }
  release :: T -> IO ()
  release t = do
    let T {serverThreads, publisherThreads, subscriberInfo} = t
    HashTable.mapM_ (cancel . snd) serverThreads
    HashTable.mapM_ (cancel . snd) publisherThreads
    HashTable.mapM_ (_unsubscribe t . fst) subscriberInfo

instance RpcTransport T where
  _serve ::
       ((Text -> IO ()) -> x -> IO ())
    -> (Text -> Maybe req)
    -> Id "Rpc"
    -> T
    -> (req -> IO x)
    -> IO ()
  _serve publisher deserialize route T { requestVars
                                       , responseVars
                                       , serverThreads
                                       } handler = do
    requestVar <-
      HashTable.lookup requestVars route >>= \case
        Nothing -> do
          requestVar <- newEmptyMVar
          HashTable.insert requestVars route requestVar
          return requestVar
        Just _ -> throw $ AlreadyServing route
    let pub id x = do
          HashTable.lookup responseVars id >>= \case
            Nothing -> panic "this is a bug" -- Panic is used here only because this is a dummy
                                             -- client used in testing. Do not use otherwise!
            Just responseVar -> putMVar responseVar x
    serverThread <-
      async . forever $ do
        (id, reqText) <- takeMVar requestVar
        case deserialize reqText of
          Nothing -> panic "this is a bug"
          Just r -> handler r >>= publisher (pub id)
    HashTable.insert serverThreads route serverThread
  _call ::
       IO (res -> IO (), IO x, IO ())
    -> (req -> Text)
    -> (Text -> IO res)
    -> Id "Rpc"
    -> T
    -> Time Second
    -> req
    -> IO x
  _call handler serialize deserializeUnsafe route T {requestVars, responseVars} _timeout req = do
    id <- newUuid
    responseVar <- newEmptyMVar
    HashTable.insert responseVars id responseVar
    (push, result, waitForDone) <- handler
    renewTimeout <- timeoutThrow' waitForDone _timeout
    responseThread <-
      async . forever $ do
        response <- takeMVar responseVar
        renewTimeout >> deserializeUnsafe response >>= push
    HashTable.lookup requestVars route >>= \case
      Nothing -> panic "this is a bug"
      Just requestVar -> putMVar requestVar (id, serialize req)
    async $ waitForDone >> cancel responseThread
    result

retrieveStream :: T -> Id "Topic" -> IO (Text -> IO (), Streamly.Serial Text)
retrieveStream T {streams} topic =
  HashTable.lookup streams topic >>= \case
    Just stream -> return stream
    Nothing -> do
      (push, _, results) <- nonRepeatablePushStream
      HashTable.insert streams topic (push, results)
      return (push, results)

instance PubSubTransport T where
  _publish :: (a -> Text) -> Id "Topic" -> T -> Streamly.Serial a -> IO ()
  _publish serialize topic t as = do
    let T {publisherThreads} = t
    HashTable.lookup publisherThreads topic >>= \case
      Nothing -> do
        (push, _) <- retrieveStream t topic
        publisherThread <- async $ Streamly.mapM_ (push . serialize) as
        HashTable.insert publisherThreads topic publisherThread
      Just _ -> throw $ AlreadyPublishing topic
  _subscribe :: (Text -> IO a) -> Id "Topic" -> T -> IO (Id "Subscriber", Streamly.Serial a)
  _subscribe deserializeUnsafe topic t = do
    let T {subscriberInfo} = t
    (_, stream) <- retrieveStream t topic
    (push, close, results) <- repeatableStream
    subscriberId <- newUuid
    subscriberThread <-
      async $ Streamly.mapM_ (\msg -> deserializeUnsafe msg >>= push) stream
    HashTable.insert subscriberInfo subscriberId (subscriberThread, close)
    return (subscriberId, results)
  unsubscribe :: T -> Id "Subscriber" -> IO ()
  unsubscribe = _unsubscribe