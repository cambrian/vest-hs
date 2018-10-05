module Bridge.PubSub.Publisher
  ( module Bridge.PubSub.Publisher
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

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
  AlreadyPublishing (Namespaced' "TopicName")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Streams spec where
  Streams (Topic _ _ _ a) = Streamly.Serial a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)

class (Service service, PubSubTransport transport) =>
      Publisher service spec transport
  where
  publish :: Streams spec -> Proxy (spec, transport) -> transport -> IO ()

instance ( HasUniqueTopics (a
                            :<|> b)
         , Publisher service a transport
         , Publisher service b transport
         ) =>
         Publisher service (a
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
    publish @service aStreams (Proxy :: Proxy (a, transport)) transport
    publish @service bStreams (Proxy :: Proxy (b, transport)) transport

instance ( Service service
         , Serializable f a
         , KnownSymbol name
         , PubSubTransport transport
         ) =>
         Publisher service (Topic service f name a) transport where
  publish ::
       Streamly.Serial a
    -> Proxy (Topic service f name a, transport)
    -> transport
    -> IO ()
  publish stream _ =
    _publish
      (\send -> Streamly.mapM_ (send . serialize' @f) stream)
      (namespace @service $ proxyText' (Proxy :: Proxy name))
