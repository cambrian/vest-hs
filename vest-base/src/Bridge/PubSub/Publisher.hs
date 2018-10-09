module Bridge.PubSub.Publisher
  ( module Bridge.PubSub.Publisher
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest.Prelude

type family Topics spec where
  Topics (Topic _ _ (name :: Symbol) _) = '[ name]
  Topics (a
          :<|> b) = Topics a :++ Topics b

type family NubTopics spec where
  NubTopics (Topic _ _ (name :: Symbol) _) = '[ name]
  NubTopics (a
             :<|> b) = Nub (NubTopics a :++ NubTopics b)

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

data PubSubPublisherException =
  AlreadyPublishing (NamespacedText' "TopicName")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Streams spec where
  Streams (Topic _ _ _ a) = Streamly.Serial a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)

class (HasNamespace t, PubSubTransport transport) =>
      Publisher t spec transport
  where
  publish :: Streams spec -> Proxy (spec, transport) -> transport -> IO ()

instance ( HasUniqueTopics (a
                            :<|> b)
         , Publisher t a transport
         , Publisher t b transport
         ) =>
         Publisher t (a
                      :<|> b) transport where
  publish ::
       (Streams a
        :<|> Streams b)
    -> Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> IO ()
  publish (aStreams :<|> bStreams) _ transport = do
    publish @t aStreams (Proxy :: Proxy (a, transport)) transport
    publish @t bStreams (Proxy :: Proxy (b, transport)) transport

instance ( HasNamespace t
         , Serializable f a
         , KnownSymbol name
         , PubSubTransport transport
         ) =>
         Publisher t (Topic t f name a) transport where
  publish ::
       Streamly.Serial a
    -> Proxy (Topic t f name a, transport)
    -> transport
    -> IO ()
  publish stream _ =
    _publish
      (\send -> Streamly.mapM_ (send . serialize' @f) stream)
      (namespaced' @t $ proxyText' (Proxy :: Proxy name))
