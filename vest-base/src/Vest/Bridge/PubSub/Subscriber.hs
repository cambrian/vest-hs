module Vest.Bridge.PubSub.Subscriber
  ( module Vest.Bridge.PubSub.Subscriber
  ) where

import Vest.Bridge.PubSub.Prelude
import qualified Streamly
import Vest.Prelude

-- The bound streams close iff unsubscribe is called.
type family SubscriberBindings spec where
  SubscriberBindings () = ()
  SubscriberBindings (Topic _ _ _ _ a) = ( IO' "Unsubscribe" ()
                                         , Streamly.Serial a)
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

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , Deserializable fmt a
         , KnownSymbol name
         ) =>
         Subscriber t (Topic fmt t transport name a) where
  subscribe ::
       t
    -> Proxy (Topic fmt t transport name a)
    -> IO (IO' "Unsubscribe" (), Streamly.Serial a)
  subscribe t _ = do
    (push_, Tagged close, stream) <- pushStream
    let push = push_ <=< deserializeUnsafe' @fmt
    Tagged unsubscribe_ <-
      _subscribe
        push
        (namespaced' @t $ proxyText' (Proxy :: Proxy name))
        (pubSubTransport @transport t)
    let unsubscribe = Tagged $ close >> unsubscribe_
    return (unsubscribe, stream)
