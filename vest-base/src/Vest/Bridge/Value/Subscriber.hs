module Vest.Bridge.Value.Subscriber
  ( module Vest.Bridge.Value.Subscriber
  ) where

import Vest.Bridge.Value.Prelude
import Vest.Prelude

type family SubscriberBindings spec where
  SubscriberBindings () = ()
  SubscriberBindings (ValueTopic_ _ _ _ _ a) = Stream ValueBuffer a
  -- ^ Returns (stream, read). Read blocks until the first value is received. Consumers of a
  -- value stream should not count on receiving every update.
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
  subscribe t _ = do
    aStreams <- subscribe t (Proxy :: Proxy a)
    bStreams <- subscribe t (Proxy :: Proxy b)
    return $ aStreams :<|> bStreams

instance ( HasNamespace service
         , HasValueTransport transport t
         , Deserializable fmt a
         , KnownSymbol name
         , Eq a
         ) =>
         Subscriber t (ValueTopic_ fmt service transport name a) where
  subscribe t _ = do
    (pusher, stream) <- newStream @ValueBuffer
    subscribeValue
      (valueTransport @transport t)
      (serialize' @'Pretty $ namespaced @service (Proxy :: Proxy name))
      (pushStream pusher <=< deserializeUnsafe' @fmt)
    return stream