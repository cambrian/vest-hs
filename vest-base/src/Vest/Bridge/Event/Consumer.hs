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
  Consumers (Event_ _ _ _ _ a) = (IO (IndexOf a), a -> IO ())
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
         , HasEventTransport transport t
         , HasRedisConnection t
         , KnownEventName name
         , Indexable a
         ) =>
         Consumer t (Event_ fmt server transport name a) where
  consume t _ (getStartIndex, f) = do
    let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
        lockId = Tagged $ untag eventName <> "/consumer/" <> namespace @t
    void . asyncThrows $
      withDistributedLock t lockId $ do
        let materialize =
              makeClient
                t
                (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt server transport name a)))
        (writer, stream) <- newStream
        subscribeEvents
          (eventTransport @transport t)
          eventName
          (writeStream writer <=< deserializeUnsafe' @fmt)
        startIndex <- getStartIndex
        gapFilledStream materialize startIndex stream >>= consumeStream f
