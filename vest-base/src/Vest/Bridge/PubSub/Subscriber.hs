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
  SubscriberBindings (Topic_ _ _ _ _ 'Value a) = Stream a
  SubscriberBindings (Topic_ _ _ _ _ 'Event a) = IndexOf a -> Stream a -- lastIndexSeen -> events
  SubscriberBindings (a
                      :<|> b) = (SubscriberBindings a
                                 :<|> SubscriberBindings b)

-- | SubscribeOne (non-streaming version) is deliberately unimplemented, because RabbitMQ does not
-- support message history. Candidate solutions are building a separate pub/sub system on Kafka (or
-- similar), or adding Cassandra to RabbitMQ. For now, you can use makeStreamVar in conjunction
-- with subscribe to get one-off values from a published topic.
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
         Subscriber t (Topic_ fmt service transport name 'Value a) where
  subscribe ::
       t -> Proxy (Topic_ fmt service transport name 'Value a) -> IO (Stream a)
  subscribe t _ = do
    (push, _close, stream) <- pushStream
    _ <-
      _subscribe
        (push <=< deserializeUnsafe' @fmt)
        1
        (namespaced' @service $ symbolText' (Proxy :: Proxy name))
        (pubSubTransport @transport t)
    return stream

instance ( HasNamespace service
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Deserializable fmt a
         , KnownSymbol name
         , Indexable a
         ) =>
         Subscriber t (Topic_ fmt service transport name 'Event a) where
  subscribe ::
       t
    -> Proxy (Topic_ fmt service transport name 'Event a)
    -> IO (IndexOf a -> Stream a)
  subscribe t _ = do
    let topicName = namespaced' @service $ symbolText' (Proxy :: Proxy name)
    let lockId = retag $ topicName <> "/subscriber"
    (push, _close, stream) <- pushStream
    void . async $
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $
      _subscribe
        (push <=< deserializeUnsafe' @fmt)
        20
        (namespaced' @service $ symbolText' (Proxy :: Proxy name))
        (pubSubTransport @transport t)
    return $ \lastIndexSeen ->
      Stream.dropWhile ((lastIndexSeen >) . index) stream
