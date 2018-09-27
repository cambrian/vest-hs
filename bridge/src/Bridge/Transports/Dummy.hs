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
  { requestVars :: HashTable (Text' "Route") (MVar ( Text' "RequestId" -- Route to full request.
                                                   , Headers
                                                   , Text' "Request"))
  , responseVars :: HashTable (Text' "RequestId") (MVar (Text' "Response")) -- Call ID to response.
  , serverThreads :: HashTable (Text' "Route") (Async ()) -- List of serving threads.
  , publisherThreads :: HashTable (Text' "TopicName") (Async ()) -- List of publisher threads.
  , streams :: HashTable (Text' "TopicName") ( Text -> IO ()
                                             , Streamly.Serial Text)
  , subscriberInfo :: HashTable (Text' "SubscriberId") (Async (), IO ()) -- ID to (thread, close).
  }

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
       ((Text' "Response" -> IO ()) -> Headers -> Text' "Request" -> IO ())
    -> Text' "Route"
    -> T
    -> IO ()
  _serve processor route T {requestVars, responseVars, serverThreads} = do
    requestVar <-
      HashTable.lookup requestVars route >>= \case
        Nothing -> do
          requestVar <- newEmptyMVar
          HashTable.insert requestVars route requestVar
          return requestVar
        Just _ -> throw $ AlreadyServing route
    let send id resText = do
          HashTable.lookup responseVars id >>= \case
            Nothing -> panic "this is a bug" -- Panic is used here only because this is a dummy
                                             -- client used in testing. Do not use otherwise!
            Just responseVar -> putMVar responseVar resText
    serverThread <-
      async . forever $ do
        (id, headers, reqText) <- takeMVar requestVar
        processor (send id) headers reqText
    HashTable.insert serverThreads route serverThread
  _call ::
       ((Headers -> Text' "Request" -> IO ()) -> Time Second -> Headers -> req -> IO ( Text' "Response" -> IO ()
                                                                                     , IO x
                                                                                     , IO ()))
    -> Text' "Route"
    -> T
    -> Time Second
    -> Headers
    -> req
    -> IO x
  _call processor route T {requestVars, responseVars} _timeout headers req = do
    id <- newUUID
    let send _headers reqText =
          HashTable.lookup requestVars route >>= \case
            Nothing -> panic "this is a bug"
            Just requestVar -> putMVar requestVar (id, _headers, reqText)
    (push, result, waitForDone) <- processor send _timeout headers req
    responseVar <- newEmptyMVar
    HashTable.insert responseVars id responseVar
    -- TODO: Track these threads?
    responseThread <-
      async . forever $ do
        response <- takeMVar responseVar
        push response
    async $ waitForDone >> cancel responseThread
    result

retrieveStream ::
     T -> Text' "TopicName" -> IO (Text -> IO (), Streamly.Serial Text)
retrieveStream T {streams} topic =
  HashTable.lookup streams topic >>= \case
    Just stream -> return stream
    Nothing -> do
      (push, _, results) <- singleUsePushStream
      HashTable.insert streams topic (push, results)
      return (push, results)

_unsubscribe :: T -> Text' "SubscriberId" -> IO ()
_unsubscribe T {subscriberInfo} subscriberId = do
  let maybeUnsubscribe =
        (>|>| \(thread, close) -> do
                cancel thread
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
  _publish processor topic as t = do
    let T {publisherThreads} = t
    HashTable.lookup publisherThreads topic >>= \case
      Nothing -> do
        (push, _) <- retrieveStream t topic
        publisherThread <- async $ processor (\(Text' a) -> push a) as
        HashTable.insert publisherThreads topic publisherThread
      Just _ -> throw $ AlreadyPublishing topic
  _subscribe ::
       (Text' "a" -> IO ())
    -> IO ()
    -> Text' "TopicName"
    -> T
    -> IO (Text' "SubscriberId")
  _subscribe push close topic t = do
    let T {subscriberInfo} = t
    (_, stream) <- retrieveStream t topic
    subscriberThread <- async $ Streamly.mapM_ (push . Text') stream
    subscriberId <- newUUID
    HashTable.insert subscriberInfo subscriberId (subscriberThread, close)
    return subscriberId
  unsubscribe :: T -> Text' "SubscriberId" -> IO ()
  unsubscribe = _unsubscribe
