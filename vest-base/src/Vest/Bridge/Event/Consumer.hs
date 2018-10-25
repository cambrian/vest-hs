module Vest.Bridge.Event.Consumer
  ( Consumers
  , Consumer(..)
  ) where

import qualified Stream
import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.Bridge.Variable
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

-- TODO: return IO Void. Requires observeFromIndex to be IO Void
type family Consumers spec where
  Consumers () = ()
  Consumers (Event_ _ _ _ _ _ a) = IO (IndexOf a) -> (a -> IO ()) -> IO (Async ())
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
         , HasRpcTransport rpcTransport t
         , HasVariableTransport varTransport t
         , HasRedisConnection t
         , KnownEventName name
         , Indexable a
         ) =>
         Consumer t (Event_ fmt server rpcTransport varTransport name a) where
  consume t _ = do
    let lockId =
          Tagged $
          symbolText (Proxy :: Proxy (PrefixedEventName name)) <> "/consumer/" <>
          namespace @t
    return $ \getStartIndex callback ->
      async $
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $ do
        let materialize =
              makeClient
                t
                (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt server rpcTransport varTransport name a)))
        (stream, _) <-
          subscribe
            t
            (Proxy :: Proxy (EventVariable (Event_ fmt server rpcTransport varTransport name a)))
        getStartIndex >>= gapFilledStream stream materialize callback

gapFilledStream ::
     (Indexable a)
  => Stream a
  -> (IndexOf a -> IO a)
  -> (a -> IO ())
  -> IndexOf a
  -> IO ()
gapFilledStream stream materializer f startIndex = do
  let process a startIdx =
        if startIdx < index a
          then do
            a' <- materializer startIdx
            f a'
            process a $ succ startIdx
          else do
            f a
            return $ succ $ index a
  void $ Stream.foldrM process startIndex stream
