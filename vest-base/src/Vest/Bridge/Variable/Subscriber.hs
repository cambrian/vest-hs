module Vest.Bridge.Variable.Subscriber
  ( module Vest.Bridge.Variable.Subscriber
  ) where

import Vest.Bridge.Variable.Prelude
import Vest.Prelude

type family SubscriberBindings spec where
  SubscriberBindings () = ()
  SubscriberBindings (Variable_ _ _ _ _ a) = (Stream a, STM a)
  -- ^ Returns (stream, read). Read blocks until the first value is received. Consumers of a
  -- variable stream should not count on receiving every update.
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
         , HasVariableTransport transport t
         , Deserializable fmt a
         , KnownSymbol name
         ) =>
         Subscriber t (Variable_ fmt service transport name a) where
  subscribe t _ = do
    (push, _, stream, read) <- pushStream
    subscribeRaw
      (variableTransport @transport t)
      (serialize' @'Pretty $ namespaced @service (Proxy :: Proxy name))
      (push <=< deserializeUnsafe' @fmt)
    return (stream, read)
