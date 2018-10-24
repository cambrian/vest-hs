module Vest.Bridge.Event.Consumer
  ( Consumers
  , Consumer(..)
  ) where

import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.Bridge.Variable
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

-- TODO: return IO Void. Requires observeFromIndex to be IO Void
type family Consumers spec where
  Consumers () = ()
  Consumers (Event_ _ _ _ _ _ a) = (a -> IO ()) -> IndexOf a -> IO (Async ())
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
          symbolText (Proxy :: Proxy name) <> "/consumer/" <> namespace @t
    return $ \callback consumeFrom ->
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
        observeFromIndex stream materialize callback consumeFrom
