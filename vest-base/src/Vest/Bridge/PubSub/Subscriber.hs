module Vest.Bridge.PubSub.Subscriber
  ( module Vest.Bridge.PubSub.Subscriber
  ) where

import qualified Stream
import Vest.Bridge.PubSub.Prelude
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family SubscriberBindings spec where
  SubscriberBindings () = ()
  SubscriberBindings (Topic_ _ 'Value _ _ _ a) = STM a
  -- ^ Returns an STM operation to get the most recent value. The STM operation will block until
  -- the first value is received.
  SubscriberBindings (Topic_ _ 'Event _ _ _ a) = Maybe (IndexOf a) -> Stream a
  -- ^ Maybe LastIndexSeen -> events
  -- Event topic subscribes is locked so that only 1 process can subscribe to an event stream at a
  -- time. subscribe will return immediately even if the lock is not acquired, however the stream
  -- will not receive events until it is.
  SubscriberBindings (a
                      :<|> b) = (SubscriberBindings a
                                 :<|> SubscriberBindings b)

class Subscriber t spec where
  subscribe :: t -> Proxy spec -> IO (SubscriberBindings spec)

instance Subscriber t () where
  subscribe _ _ = return ()

instance (Subscriber t a, Subscriber t b) =>
         Subscriber t (a
                       :<|> b) where
  subscribe ::
       t
    -> Proxy (a
              :<|> b)
    -> IO (SubscriberBindings a
           :<|> SubscriberBindings b)
  subscribe t _ = do
    aStreams <- subscribe t (Proxy :: Proxy a)
    bStreams <- subscribe t (Proxy :: Proxy b)
    return $ aStreams :<|> bStreams

instance ( HasNamespace service
         , HasPubSubTransport transport t
         , Deserializable fmt a
         , KnownSymbol name
         ) =>
         Subscriber t (Topic_ fmt 'Value service transport name a) where
  subscribe ::
       t -> Proxy (Topic_ fmt 'Value service transport name a) -> IO (STM a)
  subscribe t _ = do
    let rawTopicName =
          show' $ namespaced @service $ symbolText' (Proxy :: Proxy name)
        transport = pubSubTransport @transport t
    initTopic transport rawTopicName Value
    var <- newTVarIO Nothing
    subscribe_
      transport
      rawTopicName
      ((atomically . writeTVar var . Just) <=< deserializeUnsafe' @fmt)
    return $
      readTVar var >>= \case
        Nothing -> retry
        Just v -> return v

instance ( HasNamespace service
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Deserializable fmt a
         , KnownSymbol name
         , Indexable a
         ) =>
         Subscriber t (Topic_ fmt 'Event service transport name a) where
  subscribe ::
       t
    -> Proxy (Topic_ fmt 'Event service transport name a)
    -> IO (Maybe (IndexOf a) -> Stream a)
  subscribe t _ = do
    let rawTopicName =
          show' $ namespaced @service $ symbolText' (Proxy :: Proxy name)
        lockId = retag $ rawTopicName <> "/subscriber"
        transport = pubSubTransport @transport t
    (push, _close, stream) <- pushStream
    void . async $
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $ do
        initTopic transport rawTopicName Event
        subscribe_ transport rawTopicName (push <=< deserializeUnsafe' @fmt)
    return $ \case
      Just lastIndexSeen -> Stream.dropWhile ((lastIndexSeen >) . index) stream
      Nothing -> stream
