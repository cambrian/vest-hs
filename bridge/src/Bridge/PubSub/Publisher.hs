module Bridge.PubSub.Publisher
  ( module Bridge.PubSub.Publisher
  ) where

import Bridge.PubSub.Prelude
import qualified Streamly
import VestPrelude

type family Topics spec where
  Topics (TopicAs (f :: Format) (s :: Symbol) a) = '[ s]
  Topics (a
          :<|> b) = (Topics a) :++ (Topics b)

type family NubTopics spec where
  NubTopics (TopicAs (f :: Format) (s :: Symbol) a) = '[ s]
  NubTopics (a
             :<|> b) = Nub ((NubTopics a) :++ (NubTopics b))

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

type family Streams spec where
  Streams (TopicAs (f :: Format) (s :: Symbol) a) = Streamly.Serial a
  Streams (a
           :<|> b) = (Streams a
                      :<|> Streams b)

class (PubSubTransport transport) =>
      Publisher spec transport
  where
  publish :: Proxy (spec, transport) -> transport -> Streams spec -> IO ()

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
    -> transport
    -> (Streams a
        :<|> Streams b)
    -> IO ()
  publish _ transport (aStreams :<|> bStreams) = do
    publish (Proxy :: Proxy (a, transport)) transport aStreams
    publish (Proxy :: Proxy (b, transport)) transport bStreams

instance (KnownSymbol route, Show a, PubSubTransport transport) =>
         Publisher (Topic (route :: Symbol) a) transport where
  publish ::
       Proxy (Topic route a, transport)
    -> transport
    -> Streamly.Serial a
    -> IO ()
  publish _ = _publish show (Route $ proxyText $ (Proxy :: Proxy route))

instance (KnownSymbol route, ToJSON a, PubSubTransport transport) =>
         Publisher (TopicJSON (route :: Symbol) a) transport where
  publish ::
       Proxy (TopicJSON route a, transport)
    -> transport
    -> Streamly.Serial a
    -> IO ()
  publish _ = _publish encode (Route $ proxyText $ (Proxy :: Proxy route))
