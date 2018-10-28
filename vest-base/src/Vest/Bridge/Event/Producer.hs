module Vest.Bridge.Event.Producer
  ( Producers
  , Producer(..)
  ) where

import Vest.Bridge.Event.Prelude
import Vest.Bridge.Rpc
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family EventNames spec where
  EventNames () = '[]
  EventNames (Event_ _ _ _ (name :: Symbol) _) = '[ name]
  EventNames (a
              :<|> b) = EventNames a :++ EventNames b

type family NubEventNames spec where
  NubEventNames () = '[]
  NubEventNames (Event_ _ _ _ (name :: Symbol) _) = '[ name]
  NubEventNames (a
                 :<|> b) = Nub (NubEventNames a :++ NubEventNames b)

type HasUniqueEventNames spec = EventNames spec ~ NubEventNames spec

type family Producers spec where
  Producers () = ()
  Producers (Event_ _ t _ _ a) = (Stream QueueBuffer a, t -> IndexOf a -> IO a)
  Producers (a
             :<|> b) = (Producers a
                        :<|> Producers b)

class (HasNamespace t) =>
      Producer t spec
  where
  produce :: t -> Proxy spec -> Producers spec -> IO ()

instance HasNamespace t => Producer t () where
  produce _ _ _ = return ()

instance ( HasUniqueEventNames (a
                                :<|> b)
         , Producer t a
         , Producer t b
         ) =>
         Producer t (a
                     :<|> b) where
  produce t _ (aProducers :<|> bProducers) = do
    produce t (Proxy :: Proxy a) aProducers
    produce t (Proxy :: Proxy b) bProducers

instance ( HasNamespace t
         , HasRpcTransport transport t
         , HasEventTransport transport t
         , HasRedisConnection t
         , Deserializable fmt (IndexOf a)
         , Serializable fmt (RpcResponse a)
         , Serializable fmt a
         , KnownEventName name
         ) =>
         Producer t (Event_ fmt t transport name a) where
  produce t _ (stream, materializer) = do
    serve
      t
      (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt t transport name a)))
      materializer
    void . async $ do
      let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
          lockId = retag $ eventName <> "/producer"
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
        const $ do
        send <- publishEvents (eventTransport @transport t) eventName
        tapStream_ (send . serialize' @fmt) stream
