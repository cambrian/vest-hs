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
  publish :: t -> Proxy spec -> Streams spec -> IO ()

instance HasNamespace t => Publisher t () where
  publish _ _ _ = return ()

instance ( HasUniqueTopics (a
                            :<|> b)
         , Publisher t a
         , Publisher t b
         ) =>
         Publisher t (a
                      :<|> b) where
  publish t _ (aStreams :<|> bStreams) = do
    publish t (Proxy :: Proxy a) aStreams
    publish t (Proxy :: Proxy b) bStreams

publish_ ::
     forall fmt transport topicName t a.
     ( Serializable fmt a
     , HasNamespace t
     , HasPubSubTransport transport t
     , HasRedisConnection t
     , KnownSymbol topicName
     )
  => TopicType
  -> t
  -> Stream a
  -> IO ()
publish_ topicType t stream =
  void . async $ do
    let rawTopicName =
          serialize' @'Pretty $ namespaced @t (Proxy :: Proxy topicName)
        lockId = retag $ rawTopicName <> "/publisher"
        transport = pubSubTransport @transport t
    with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
      const $ do
      initTopic transport rawTopicName topicType
      send <- initPublisher transport rawTopicName
      Stream.mapM_ (send . serialize' @fmt) stream

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         ) =>
         Publisher t (Topic_ fmt 'Value t transport name a) where
  publish t _ = publish_ @fmt @transport @name Value t

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         ) =>
         Publisher t (Topic_ fmt 'Event t transport name a) where
  publish t _ = publish_ @fmt @transport @name Event t
