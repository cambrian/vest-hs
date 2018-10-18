module Vest.Bridge.PubSub.Publisher
  ( module Vest.Bridge.PubSub.Publisher
  ) where

import qualified Stream
import Vest.Bridge.PubSub.Prelude
import qualified Vest.DistLock as Lock
import Vest.Prelude
import qualified Vest.Redis as Redis

type family Topics spec where
  Topics () = '[]
  Topics (Topic _ _ _ (name :: Symbol) _) = '[ name]
  Topics (a
          :<|> b) = Topics a :++ Topics b

type family NubTopics spec where
  NubTopics () = '[]
  NubTopics (Topic _ _ _ (name :: Symbol) _) = '[ name]
  NubTopics (a
             :<|> b) = Nub (NubTopics a :++ NubTopics b)

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

data PubSubPublisherException =
  AlreadyPublishing (NamespacedText' "TopicName")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Streams spec where
  Streams () = ()
  Streams (Topic _ _ _ _ a) = Stream a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)
  -- Would be better to declare like this, but type-level lists cannot be used as argument types:
  -- Streams '[] = '[]
  -- Streams (Topic _ _ _ _ a ': topics) = ('[ Streamly.Serial a] :++ Streams topics)
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

instance ( HasNamespace t
         , HasPubSubTransport transport t
         , Redis.HasRedis t
         , Serializable f a
         , KnownSymbol name
         ) =>
         Publisher t (Topic f t transport name a) where
  publish :: Stream a -> t -> Proxy (Topic f t transport name a) -> IO ()
  publish stream t _ =
    void . async $ do
      let pubRoute = namespaced' @t $ symbolText' (Proxy :: Proxy name)
      with @Lock.T (Lock.defaultLock (Redis.redis t) (retag pubRoute)) $
        const $
        _publish
          (\send -> Stream.mapM_ (send . serialize' @f) stream)
          pubRoute
          (pubSubTransport @transport t)
