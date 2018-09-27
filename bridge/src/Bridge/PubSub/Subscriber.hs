module Bridge.PubSub.Subscriber
  ( module Bridge.PubSub.Subscriber
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import VestPrelude

-- The bound streams close iff unsubscribe is called.
type family SubscriberBindings spec where
  SubscriberBindings (Topic _ _ a) = (Text' "SubscriberId", Streamly.Serial a)
  SubscriberBindings (a
                      :<|> b) = (SubscriberBindings a
                                 :<|> SubscriberBindings b)

class (PubSubTransport transport) =>
      Subscriber spec transport
  where
  subscribe ::
       Proxy (spec, transport) -> transport -> IO (SubscriberBindings spec)

instance (Subscriber a transport, Subscriber b transport) =>
         Subscriber (a
                     :<|> b) transport where
  subscribe ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO (SubscriberBindings a
           :<|> SubscriberBindings b)
  subscribe _ transport = do
    aStreams <- subscribe (Proxy :: Proxy (a, transport)) transport
    bStreams <- subscribe (Proxy :: Proxy (b, transport)) transport
    return $ aStreams :<|> bStreams

instance (Deserializable f a, KnownSymbol name, PubSubTransport transport) =>
         Subscriber (Topic (f :: SerializationFormat) (name :: Symbol) a) transport where
  subscribe ::
       Proxy (Topic f name a, transport)
    -> transport
    -> IO (Text' "SubscriberId", Streamly.Serial a)
  subscribe _ transport = do
    (push_, close, stream) <- pushStream
    let push = push_ <=< deserializeUnsafe' @f
    subscriberId <-
      _subscribe push close (proxyText' (Proxy :: Proxy name)) transport
    return (subscriberId, stream)
