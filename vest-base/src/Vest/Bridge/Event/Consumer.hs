module Vest.Bridge.Event.Consumer
  ( Consumers
  , Consumer(..)
  ) where

import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

-- | Consumption begins from the provided index (inclusive).
type family Consumers spec where
  Consumers () = ()
  Consumers (Event_ _ _ _ _ a) = (IndexOf a, a -> IO ())
  Consumers (a
             :<|> b) = (Consumers a
                        :<|> Consumers b)

class Consumer t spec where
  consume :: t -> Proxy spec -> Consumers spec -> IO ()

instance Consumer t () where
  consume _ _ _ = return ()

instance (Consumer t a, Consumer t b) =>
         Consumer t (a
                     :<|> b) where
  consume t _ (aConsumers :<|> bConsumers) = do
    consume t (Proxy :: Proxy a) aConsumers
    consume t (Proxy :: Proxy b) bConsumers

-- TODO: remove locking, but adjust transport settings such that events are only
-- delivered to one instance
instance ( Client t (EventMaterializeEndpoint (Event_ fmt server transport name a))
         , Deserializable fmt a
         , HasNamespace t
         , EventTransport transport
         , Has transport t
         , Has RedisConnection t
         , KnownEventName name
         , Indexable a
         ) =>
         Consumer t (Event_ fmt server transport name a) where
  consume t _ (startIndex, f) = do
    let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
        lockId = Tagged $ untag eventName <> "/consumer/" <> namespace @t
    void . async $
      withDistributedLock t lockId $ do
        let materialize =
              makeClient
                t
                (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt server transport name a)))
        (writer, stream) <- newStream
        subscribeEvents
          (get @transport t)
          eventName
          (writeStream writer <=< deserializeUnsafe' @fmt)
        gapFilledStream materialize startIndex stream >>= consumeStream f
