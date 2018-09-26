module Bridge.PubSub.Publisher
  ( module Bridge.PubSub.Publisher
  ) where

import Bridge.Prelude
import Bridge.PubSub.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family Topics spec where
  Topics (Topic (f :: Format) (s :: Symbol) a) = '[ s]
  Topics (a
          :<|> b) = Topics a :++ Topics b

type family NubTopics spec where
  NubTopics (Topic (f :: Format) (s :: Symbol) a) = '[ s]
  NubTopics (a
             :<|> b) = Nub (NubTopics a :++ NubTopics b)

type HasUniqueTopics spec = Topics spec ~ NubTopics spec

data PubSubPublisherException =
  AlreadyPublishing (Id "Topic")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)

type family Streams spec where
  Streams (Topic (f :: Format) (s :: Symbol) a) = Streamly.Serial a
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

publishProcessor ::
     (Show a, ToJSON a)
  => Format
  -> (Text -> IO ())
  -> Streamly.Serial a
  -> IO ()
publishProcessor format send = Streamly.mapM_ (send . serializeOf format)

instance (KnownSymbol s, Show a, ToJSON a, PubSubTransport transport) =>
         Publisher (Topic 'Haskell (s :: Symbol) a) transport where
  publish ::
       Proxy (Topic 'Haskell s a, transport)
    -> Streamly.Serial a
    -> transport
    -> IO ()
  publish _ =
    _publish (publishProcessor Haskell) (Id $ proxyText (Proxy :: Proxy s))

instance (KnownSymbol s, Show a, ToJSON a, PubSubTransport transport) =>
         Publisher (Topic 'JSON (s :: Symbol) a) transport where
  publish ::
       Proxy (Topic 'JSON s a, transport)
    -> Streamly.Serial a
    -> transport
    -> IO ()
  publish _ =
    _publish (publishProcessor JSON) (Id $ proxyText (Proxy :: Proxy s))
