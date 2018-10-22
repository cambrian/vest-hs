module Vest.Bridge.PubSub.Publisher
  ( module Vest.Bridge.PubSub.Publisher
  ) where

import qualified Stream
import Vest.Bridge.PubSub.Prelude
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family Topics spec where
  Topics () = '[]
  Topics (Topic_ _ _ _ _ (name :: Symbol) _) = '[ name]
  Topics (a
          :<|> b) = Topics a :++ Topics b

type family NubTopics spec where
  NubTopics () = '[]
  NubTopics (Topic_ _ _ _ _ (name :: Symbol) _) = '[ name]
  NubTopics (a
             :<|> b) = Nub (NubTopics a :++ NubTopics b)

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

type family Streams spec where
  Streams () = ()
  Streams (Topic_ _ _ _ _ _ a) = Stream a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)
  -- Would be better to declare like this, but type-level lists cannot be used as argument types:
  -- Streams '[] = '[]
  -- Streams (Topic_ _ _ _ _ a ': topics) = ('[ Streamly.Serial a] :++ Streams topics)
  -- Watch here: https://www.reddit.com/r/haskell/comments/9n0qan/typefamily_monoid/.

class (HasNamespace t) =>
      Publisher t spec
  where
  publish :: Streams spec -> t -> Proxy spec -> IO ()

instance HasNamespace t => Publisher t () where
  publish _ _ _ = return ()

instance ( HasUniqueTopics (a
                            :<|> b)
         , Publisher t a
         , Publisher t b
         ) =>
         Publisher t (a
                      :<|> b) where
  publish ::
       (Streams a
        :<|> Streams b)
    -> t
    -> Proxy (a
              :<|> b)
    -> IO ()
  publish (aStreams :<|> bStreams) t _ = do
    publish aStreams t (Proxy :: Proxy a)
    publish bStreams t (Proxy :: Proxy b)

publish_ ::
     forall fmt transport t topicName a.
     ( Serializable fmt a
     , HasNamespace t
     , HasPubSubTransport transport t
     , HasRedisConnection t
     , KnownSymbol topicName
     )
  => TopicType
  -> Stream a
  -> t
  -> Proxy topicName
  -> IO ()
publish_ topicType stream t _ =
  void . async $ do
    let rawTopicName =
          show' $ namespaced @t $ symbolText' (Proxy :: Proxy topicName)
        lockId = retag $ rawTopicName <> "/publisher"
        transport = pubSubTransport @transport t
    with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $ do
      initTopic transport rawTopicName topicType
      send <- initPublisher rawTopicName transport
      Stream.mapM_ (send . serialize' @fmt) stream

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         ) =>
         Publisher t (Topic_ fmt 'Value t transport name a) where
  publish ::
       Stream a -> t -> Proxy (Topic_ fmt 'Value t transport name a) -> IO ()
  publish stream t _ =
    publish_ @fmt @transport Value stream t (Proxy :: Proxy name)

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         ) =>
         Publisher t (Topic_ fmt 'Event t transport name a) where
  publish ::
       Stream a -> t -> Proxy (Topic_ fmt 'Event t transport name a) -> IO ()
  publish stream t _ =
    publish_ @fmt @transport Event stream t (Proxy :: Proxy name)
