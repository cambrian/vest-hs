module Bridge.PubSub.Subscriber
  ( module Bridge.PubSub.Subscriber
  ) where

import Bridge.Prelude
import Bridge.PubSub.Prelude
import qualified Streamly
import VestPrelude

-- The bound streams close iff unsubscribe is called.
type family SubscriberBindings spec where
  SubscriberBindings (Topic (f :: Format) (s :: Symbol) a) = ( Text' "SubscriberId"
                                                             , Streamly.Serial a)
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

subscribeProcessor ::
     (Read a, FromJSON a)
  => Format
  -> IO (Text' "a" -> IO (), IO (), Streamly.Serial a)
subscribeProcessor format = do
  (_push, close, stream) <- pushStream
  let push (Text' a) = deserializeUnsafeOf format a >>= _push
  return (push, close, stream)

instance (KnownSymbol s, Read a, FromJSON a, PubSubTransport transport) =>
         Subscriber (Topic 'Haskell (s :: Symbol) a) transport where
  subscribe ::
       Proxy (Topic 'Haskell s a, transport)
    -> transport
    -> IO (Text' "SubscriberId", Streamly.Serial a)
  subscribe _ =
    _subscribe
      (subscribeProcessor Haskell)
      (Text' $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, Read a, FromJSON a, PubSubTransport transport) =>
         Subscriber (Topic 'JSON (s :: Symbol) a) transport where
  subscribe ::
       Proxy (Topic 'JSON s a, transport)
    -> transport
    -> IO (Text' "SubscriberId", Streamly.Serial a)
  subscribe _ =
    _subscribe (subscribeProcessor JSON) (Text' $ proxyText (Proxy :: Proxy s))
