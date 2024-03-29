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

type UniqueEventNames spec = EventNames spec ~ NubEventNames spec

-- | Producers is really ProducerConstructors or something like that,
-- because initializing a producer generally means beginning db writes,
-- we only want one producer to be initialized among all service instances.
-- This means binding a producer constructor from within a locked context.
type family Producers spec where
  Producers () = ()
  Producers (Event_ _ _ _ _ a) = (Stream QueueBuffer a, IndexOf a -> IO a)
  Producers (a
             :<|> b) = (Producers a
                        :<|> Producers b)

class Producer t spec where
  produce :: t -> Proxy spec -> Producers spec -> IO ()

instance Producer t () where
  produce _ _ _ = return ()

instance ( UniqueEventNames (a
                             :<|> b)
         , Producer t a
         , Producer t b
         ) =>
         Producer t (a
                     :<|> b) where
  produce t _ (aProducers :<|> bProducers) = do
    produce t (Proxy :: Proxy a) aProducers
    produce t (Proxy :: Proxy b) bProducers

instance ( Server t (EventMaterializeEndpoint (Event_ fmt t transport name a))
         , EventTransport transport
         , Has transport t
         , Has RedisConnection t
         , Serializable fmt a
         , KnownEventName name
         ) =>
         Producer t (Event_ fmt t transport name a) where
  produce t _ (eventStream, materializer) = do
    serve
      t
      (Proxy :: Proxy (EventMaterializeEndpoint (Event_ fmt t transport name a)))
      materializer
    void . async $ do
      let eventName = symbolText' (Proxy :: Proxy (PrefixedEventName name))
          lockId = retag $ eventName <> "/producer"
      withDistributedLock t lockId $ do
        send <- publishEvents (get @transport t) eventName
        consumeStream (send . serialize' @fmt) eventStream
