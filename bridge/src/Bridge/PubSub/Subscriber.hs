module Bridge.PubSub.Subscriber
  ( module Bridge.PubSub.Subscriber
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import VestPrelude

-- The bound streams close iff unsubscribe is called.
type family SubscriberBindings spec where
  SubscriberBindings (Topic _ _ _ a) = (IO' "Unsubscribe" (), Streamly.Serial a)
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

instance ( Service service
         , Deserializable f a
         , KnownSymbol name
         , PubSubTransport transport
         ) =>
         Subscriber (Topic service f name a) transport where
  subscribe ::
       Proxy (Topic service f name a, transport)
    -> transport
    -> IO (IO' "Unsubscribe" (), Streamly.Serial a)
  subscribe _ transport = do
    (push_, Tagged close, stream) <- pushStream
    let push = push_ <=< deserializeUnsafe' @f
    Tagged unsubscribe_ <-
      _subscribe
        push
        (namespace @service $ proxyText' (Proxy :: Proxy name))
        transport
    let unsubscribe = Tagged $ close >> unsubscribe_
    return (unsubscribe, stream)
