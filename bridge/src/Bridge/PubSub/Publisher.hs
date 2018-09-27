module Bridge.PubSub.Publisher
  ( module Bridge.PubSub.Publisher
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Topics spec where
  Topics (Topic _ (name :: Symbol) _) = '[ name]
  Topics (a
          :<|> b) = Topics a :++ Topics b

type family NubTopics spec where
  NubTopics (Topic _ (name :: Symbol) _) = '[ name]
  NubTopics (a
             :<|> b) = Nub (NubTopics a :++ NubTopics b)

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

data PubSubPublisherException =
  AlreadyPublishing (Text' "TopicName")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Streams spec where
  Streams (Topic _ _ a) = Streamly.Serial a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)

class (PubSubTransport transport) =>
      Publisher spec transport
  where
  publish :: Proxy (spec, transport) -> Streams spec -> transport -> IO ()

instance ( HasUniqueTopics (a
                            :<|> b)
         , Publisher a transport
         , Publisher b transport
         ) =>
         Publisher (a
                    :<|> b) transport where
  publish ::
       Proxy ( a
               :<|> b
             , transport)
    -> (Streams a
        :<|> Streams b)
    -> transport
    -> IO ()
  publish _ (aStreams :<|> bStreams) transport = do
    publish (Proxy :: Proxy (a, transport)) aStreams transport
    publish (Proxy :: Proxy (b, transport)) bStreams transport

instance (Serializable f a, KnownSymbol name, PubSubTransport transport) =>
         Publisher (Topic (f :: SerializationFormat) (name :: Symbol) a) transport where
  publish ::
       Proxy (Topic f name a, transport)
    -> Streamly.Serial a
    -> transport
    -> IO ()
  publish _ =
    _publish
      (\send -> Streamly.mapM_ (send . serialize' @f))
      (proxyText' (Proxy :: Proxy name))
