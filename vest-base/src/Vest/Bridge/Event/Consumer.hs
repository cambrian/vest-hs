module Vest.Bridge.Event.Consumer
  ( ConsumerThreads
  , Consumers
  , Consumer(..)
  ) where

import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.DistributedLock
import Vest.Logger
import Vest.Prelude
import Vest.Redis

-- | Consumption begins from the provided index (inclusive).
type family ConsumerThreads spec where
  ConsumerThreads () = ()
  ConsumerThreads (Event_ _ _ _ _ a) = Async Void
  ConsumerThreads (a
                   :<|> b) = (ConsumerThreads a
                              :<|> ConsumerThreads b)

type family Consumers spec where
  Consumers () = ()
  Consumers (Event_ _ _ _ _ a) = ( IO (IndexOf a)
                                 , Stream QueueBuffer a -> IO Void)
  Consumers (a
             :<|> b) = (Consumers a
                        :<|> Consumers b)

class Consumer t spec where
  consume :: t -> Proxy spec -> Consumers spec -> IO (ConsumerThreads spec)

instance Consumer t () where
  consume _ _ _ = return ()

instance (Consumer t a, Consumer t b) =>
         Consumer t (a
                     :<|> b) where
  consume t _ (aConsumers :<|> bConsumers) = do
    aThreads <- consume t (Proxy :: Proxy a) aConsumers
    bThreads <- consume t (Proxy :: Proxy b) bConsumers
    return $ aThreads :<|> bThreads

instance ( Serializable fmt (IndexOf a)
         , Deserializable fmt (RpcResponse a)
         , Deserializable fmt a
         , HasNamespace server
         , HasNamespace t
         , HasLogger t
         , HasRpcTransport transport t
         , HasEventTransport transport t
         , HasRedisConnection t
         , KnownEventName name
         , Indexable a
         ) =>
         Consumer t (Event_ fmt server transport name a) where
  consume t _ (getStartIndex, f) = do
    let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
        lockId = retag $ eventName <> "/consumer/" <> Tagged (namespace @t)
    async $
      withDistributedLock t lockId $ do
        (writer, stream) <- newStream
        let materialize =
              makeClient
                t
                (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt server transport name a)))
        subscribeEvents
          (eventTransport @transport t)
          eventName
          (writeStream writer <=< deserializeUnsafe' @fmt)
        getStartIndex >>= gapFilledStream stream materialize >>= f
