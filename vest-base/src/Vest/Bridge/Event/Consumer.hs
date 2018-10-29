module Vest.Bridge.Event.Consumer
  ( Consumers
  , Consumer(..)
  ) where

import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family Consumers spec where
  Consumers () = ()
  Consumers (Event_ _ _ _ _ a) = IO (IndexOf a) -> (Stream QueueBuffer a -> IO ()) -> IO (Async ())
  Consumers (a
             :<|> b) = (Consumers a
                        :<|> Consumers b)

class Consumer t spec where
  consume :: t -> Proxy spec -> IO (Consumers spec)

instance Consumer t () where
  consume _ _ = return ()

instance (Consumer t a, Consumer t b) =>
         Consumer t (a
                     :<|> b) where
  consume t _ = do
    consumerA <- consume t (Proxy :: Proxy a)
    consumerB <- consume t (Proxy :: Proxy b)
    return $ consumerA :<|> consumerB

instance ( Serializable fmt (IndexOf a)
         , Deserializable fmt (RpcResponse a)
         , Deserializable fmt a
         , HasNamespace server
         , HasNamespace t
         , HasRpcTransport transport t
         , HasEventTransport transport t
         , HasRedisConnection t
         , KnownEventName name
         , Indexable a
         ) =>
         Consumer t (Event_ fmt server transport name a) where
  consume t _ = do
    let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
        lockId = retag $ eventName <> "/consumer/" <> Tagged (namespace @t)
    return $ \getStartIndex f ->
      async $
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $ do
        (pusher, stream) <- newStream
        let materialize =
              makeClient
                t
                (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt server transport name a)))
        subscribeEvents
          (eventTransport @transport t)
          eventName
          (pushStream pusher <=< deserializeUnsafe' @fmt)
        getStartIndex >>= gapFilledStream stream materialize >>= f

gapFilledStream ::
     Indexable a
  => Stream QueueBuffer a
  -> (IndexOf a -> IO a)
  -> IndexOf a
  -> IO (Stream QueueBuffer a)
gapFilledStream stream materializer startIndex = do
  (pusher, masterStream) <- newStream
  let f a idx =
        if idx < index a
          then do
            a' <- materializer idx
            void $ pushStream pusher a'
            f a $ succ idx
          else do
            void $ pushStream pusher a
            return $ succ $ index a
  void . async $ foldMStream f startIndex stream >> closeStream pusher
  return masterStream
