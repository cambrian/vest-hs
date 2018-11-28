module Vest.Bridge.Value.Subscriber
  ( SubscriberBindings
  , Subscriber(..)
  , MissingValueException(..)
  ) where

import Vest.Bridge.Value.Prelude
import Vest.Prelude

-- TODO: throw this exception when a subscription is made to an empty value
data MissingValueException topic =
  MissingValueException
  deriving (Eq, Show, Exception)

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
         , ValueTransport transport
         , Has transport t
         , Deserializable fmt a
         , KnownSymbol name
         , Eq a
         ) =>
         Subscriber t (ValueTopic_ fmt service transport name a) where
  subscribe t _ = do
    (writer, stream) <- newStream @ValueBuffer
    subscribeValue
      (get @transport t)
      (serialize' @'Pretty $ Namespaced @service (Proxy :: Proxy name))
      (writeStream writer <=< deserializeUnsafe' @fmt)
    return stream
