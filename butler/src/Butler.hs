module Butler
  ( Format(..)
  , Server
  , Server'
  , Client
  , Client'
  , Publisher
  , Publisher'
  , Subscriber
  , Subscriber'
  , Protocol
  , Publishing
  , ProtocolJSON
  , PublishingJSON
  , (:<|>)(..)
  , makeServer
  , makeServer'
  , makeClient
  , makeClient'
  , makePublisher
  , makePublisher'
  , makeSubscriber
  , makeSubscriber'
  , HasServer
  , HasClient
  , HasPublisher
  , HasSubscriber
  ) where

import qualified Data.Text as Text
import GHC.TypeLits
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

symbolToRoute :: (KnownSymbol a) => Proxy a -> Route
symbolToRoute = Route . Text.pack . symbolVal

symbolToStreamRoute :: (KnownSymbol a) => Proxy a -> Route
symbolToStreamRoute = Route . Text.append "STREAM::" . Text.pack . symbolVal

-- This library can also throw ReadExceptions and DecodeExceptions.
data ButlerException
  = InternalButlerException Text
  | MalformedRoute Text
  | NoSuchRoute Text
  deriving ( Eq
           , Ord
           , Show
           , Read
           , Typeable
           , Generic
           , Exception
           , Hashable
           , FromJSON
           , ToJSON
           )

-- API specification DSL.
data a :<|> b =
  a :<|> b

infixr 8 :<|>

data Format
  = Haskell
  | JSON

data PublishingAs (f :: Format) (s :: k) (a :: *)

type Publishing = PublishingAs 'Haskell

type PublishingJSON = PublishingAs 'JSON

data ProtocolAs (f :: Format) (s :: k) (a :: *) (b :: *)

type Protocol = ProtocolAs 'Haskell

type ProtocolJSON = ProtocolAs 'JSON

-- The Server type families.
type family Server spec :: *

type family Server' spec :: *

type instance Server (ProtocolAs (f :: Format) (s :: Symbol) a b) =
     a -> IO b

type instance Server' (ProtocolAs (f :: Format) (s :: Symbol) a b)
     = a -> IO (Streamly.Serial b)

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server' (a :<|> b) = Server' a :<|> Server' b

-- Used to route requests to the right handler.
-- Apostrophe versions are streaming, and direct routes must be served separately from streaming
-- routes (for clarity in implementation). Params are API and function that takes a path and
-- wrapped handler (processing requests via the handler).
class HasServer spec where
  wrap :: Proxy spec -> Server spec -> (Text -> IO Text)
  wrap' :: Proxy spec -> Server' spec -> (Text -> IO (Streamly.Serial Text))
  makeServer ::
       Proxy spec
    -> Server spec
    -> (Route -> (Text -> IO Text) -> IO tag)
    -> IO [tag]
  makeServer' ::
       Proxy spec
    -> Server' spec
    -> (Route -> (Text -> IO (Streamly.Serial Text)) -> IO tag)
    -> IO [tag]

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  wrap :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> (Text -> IO Text)
  wrap _ _ = panic "unimplemented"
  wrap' ::
       Proxy (a :<|> b)
    -> (Server' a :<|> Server' b)
    -> (Text -> IO (Streamly.Serial Text))
  wrap' _ _ = panic "unimplemented"
  makeServer ::
       Proxy (a :<|> b)
    -> (Server a :<|> Server b)
    -> (Route -> (Text -> IO Text) -> IO tag)
    -> IO [tag]
  makeServer _ (handlerA :<|> handlerB) runner = do
    tagsA <- makeServer (Proxy :: Proxy a) handlerA runner
    tagsB <- makeServer (Proxy :: Proxy b) handlerB runner
    return $ tagsA ++ tagsB
  makeServer' ::
       Proxy (a :<|> b)
    -> (Server' a :<|> Server' b)
    -> (Route -> (Text -> IO (Streamly.Serial Text)) -> IO tag)
    -> IO [tag]
  makeServer' _ (handlerA :<|> handlerB) runner = do
    tagsA <- makeServer' (Proxy :: Proxy a) handlerA runner
    tagsB <- makeServer' (Proxy :: Proxy b) handlerB runner
    return $ tagsA ++ tagsB

instance (KnownSymbol s, Read a, Show b) =>
         HasServer (Protocol (s :: Symbol) a b) where
  wrap :: Proxy (Protocol s a b) -> (a -> IO b) -> (Text -> IO Text)
  wrap _ handler =
    \request -> do
      req <- readUnsafe request
      result <- handler req
      return $ show result
  wrap' ::
       Proxy (Protocol s a b)
    -> (a -> IO (Streamly.Serial b))
    -> (Text -> IO (Streamly.Serial Text))
  wrap' _ handler =
    \request -> do
      req <- readUnsafe request
      results <- handler req
      return $ Streamly.map show results
  makeServer ::
       Proxy (Protocol s a b)
    -> (a -> IO b)
    -> (Route -> (Text -> IO Text) -> IO tag)
    -> IO [tag]
  makeServer proxy handler runner = do
    tag <- runner (symbolToRoute (Proxy :: Proxy s)) (wrap proxy handler)
    return [tag]
  makeServer' ::
       Proxy (Protocol s a b)
    -> (a -> IO (Streamly.Serial b))
    -> (Route -> (Text -> IO (Streamly.Serial Text)) -> IO tag)
    -> IO [tag]
  makeServer' proxy handler runner = do
    tag <- runner (symbolToStreamRoute (Proxy :: Proxy s)) (wrap' proxy handler)
    return [tag]

instance (KnownSymbol s, FromJSON a, ToJSON b) =>
         HasServer (ProtocolJSON (s :: Symbol) a b) where
  wrap :: Proxy (ProtocolJSON s a b) -> (a -> IO b) -> (Text -> IO Text)
  wrap _ handler =
    \request -> do
      req <- decodeUnsafe request
      result <- handler req
      return $ encodeToText result
  wrap' ::
       Proxy (ProtocolJSON s a b)
    -> (a -> IO (Streamly.Serial b))
    -> (Text -> IO (Streamly.Serial Text))
  wrap' _ handler =
    \request -> do
      req <- decodeUnsafe request
      results <- handler req
      return $ Streamly.map encodeToText results
  makeServer ::
       Proxy (ProtocolJSON s a b)
    -> (a -> IO b)
    -> (Route -> (Text -> IO Text) -> IO tag)
    -> IO [tag]
  makeServer proxy handler runner = do
    tag <- runner (symbolToRoute (Proxy :: Proxy s)) (wrap proxy handler)
    return [tag]
  makeServer' ::
       Proxy (ProtocolJSON s a b)
    -> (a -> IO (Streamly.Serial b))
    -> (Route -> (Text -> IO (Streamly.Serial Text)) -> IO tag)
    -> IO [tag]
  makeServer' proxy handler runner = do
    tag <- runner (symbolToStreamRoute (Proxy :: Proxy s)) (wrap' proxy handler)
    return [tag]

-- The Client type families.
type family Client backend spec :: *

type family Client' backend spec :: *

type instance
     Client backend (ProtocolAs (f :: Format) (s :: Symbol) a b) =
     Time Second -> backend -> a -> IO b

type instance
     Client' backend (ProtocolAs (f :: Format) (s :: Symbol) a b) =
     Time Second -> backend -> a -> IO (Streamly.Serial b)

type instance Client backend (a :<|> b) =
     Client backend a :<|> Client backend b

type instance Client' backend (a :<|> b) =
     Client' backend a :<|> Client' backend b

-- Used to generate client functions for an API (direct and streaming versions).
-- Param is request publisher.
class HasClient backend spec where
  makeClient ::
       Proxy spec
    -> (Time Second -> backend -> Route -> Text -> IO Text)
    -> Client backend spec
  makeClient' ::
       Proxy spec
    -> (Time Second -> backend -> Route -> Text -> IO (Streamly.Serial Text))
    -> Client' backend spec

instance (HasClient backend a, HasClient backend b) =>
         HasClient backend (a :<|> b) where
  makeClient ::
       Proxy (a :<|> b)
    -> (Time Second -> backend -> Route -> Text -> IO Text)
    -> Client backend a :<|> Client backend b
  makeClient _ publish =
    makeClient (Proxy :: Proxy a) publish :<|>
    makeClient (Proxy :: Proxy b) publish
  makeClient' ::
       Proxy (a :<|> b)
    -> (Time Second -> backend -> Route -> Text -> IO (Streamly.Serial Text))
    -> Client' backend a :<|> Client' backend b
  makeClient' _ publish =
    makeClient' (Proxy :: Proxy a) publish :<|>
    makeClient' (Proxy :: Proxy b) publish

instance (KnownSymbol s, Show a, Read b) =>
         HasClient backend (Protocol (s :: Symbol) a b) where
  makeClient ::
       Proxy (Protocol s a b)
    -> (Time Second -> backend -> Route -> Text -> IO Text)
    -> (Time Second -> backend -> a -> IO b)
  makeClient _ publish =
    \_timeout backend req -> do
      result <-
        publish _timeout backend (symbolToRoute (Proxy :: Proxy s)) (show req)
      readUnsafe result
  makeClient' ::
       Proxy (Protocol s a b)
    -> (Time Second -> backend -> Route -> Text -> IO (Streamly.Serial Text))
    -> (Time Second -> backend -> a -> IO (Streamly.Serial b))
  makeClient' _ publish =
    \_timeout backend req -> do
      results <-
        publish
          _timeout
          backend
          (symbolToStreamRoute (Proxy :: Proxy s))
          (show req)
      return $ Streamly.mapM readUnsafe results

instance (KnownSymbol s, ToJSON a, FromJSON b) =>
         HasClient backend (ProtocolJSON (s :: Symbol) a b) where
  makeClient ::
       Proxy (ProtocolJSON s a b)
    -> (Time Second -> backend -> Route -> Text -> IO Text)
    -> (Time Second -> backend -> a -> IO b)
  makeClient _ publish =
    \_timeout backend req -> do
      result <-
        publish
          _timeout
          backend
          (symbolToRoute (Proxy :: Proxy s))
          (encodeToText req)
      decodeUnsafe result
  makeClient' ::
       Proxy (ProtocolJSON s a b)
    -> (Time Second -> backend -> Route -> Text -> IO (Streamly.Serial Text))
    -> (Time Second -> backend -> a -> IO (Streamly.Serial b))
  makeClient' _ publish =
    \_timeout backend req -> do
      results <-
        publish
          _timeout
          backend
          (symbolToStreamRoute (Proxy :: Proxy s))
          (encodeToText req)
      return $ Streamly.mapM decodeUnsafe results

-- The Publisher type families.
type family Publisher backend spec :: *

type family Publisher' backend spec :: *

type instance
     Publisher backend (PublishingAs (f :: Format) (s :: Symbol) a) =
     backend -> a -> IO ()

type instance
     Publisher' backend (PublishingAs (f :: Format) (s :: Symbol) a) =
     backend -> (Streamly.Serial a) -> IO ()

type instance Publisher backend (a :<|> b) =
     Publisher backend a :<|> Publisher backend b

type instance Publisher' backend (a :<|> b) =
     Publisher' backend a :<|> Publisher' backend b

-- Used to generate publish functions for an API (direct and streaming versions).
-- Param is request publisher.
class HasPublisher backend spec where
  makePublisher ::
       Proxy spec
    -> (backend -> Route -> Text -> IO ())
    -> Publisher backend spec
  makePublisher' ::
       Proxy spec
    -> (backend -> Route -> Text -> IO ())
    -> Publisher' backend spec

instance (HasPublisher backend a, HasPublisher backend b) =>
         HasPublisher backend (a :<|> b) where
  makePublisher ::
       Proxy (a :<|> b)
    -> (backend -> Route -> Text -> IO ())
    -> Publisher backend a :<|> Publisher backend b
  makePublisher _ publish =
    makePublisher (Proxy :: Proxy a) publish :<|>
    makePublisher (Proxy :: Proxy b) publish
  makePublisher' ::
       Proxy (a :<|> b)
    -> (backend -> Route -> Text -> IO ())
    -> Publisher' backend a :<|> Publisher' backend b
  makePublisher' _ publish =
    makePublisher' (Proxy :: Proxy a) publish :<|>
    makePublisher' (Proxy :: Proxy b) publish

instance (KnownSymbol s, Show a) =>
         HasPublisher backend (Publishing (s :: Symbol) a) where
  makePublisher ::
       Proxy (Publishing s a)
    -> (backend -> Route -> Text -> IO ())
    -> (backend -> a -> IO ())
  makePublisher _ publish =
    \backend req -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      publish backend path (show req)
  makePublisher' ::
       Proxy (Publishing s a)
    -> (backend -> Route -> Text -> IO ())
    -> (backend -> Streamly.Serial a -> IO ())
  makePublisher' _ publish =
    \backend req -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      Streamly.mapM_ (publish backend path . show) req

instance (KnownSymbol s, ToJSON a) =>
         HasPublisher backend (PublishingJSON (s :: Symbol) a) where
  makePublisher ::
       Proxy (PublishingJSON s a)
    -> (backend -> Route -> Text -> IO ())
    -> (backend -> a -> IO ())
  makePublisher _ publish =
    \backend req -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      publish backend path (encodeToText req)
  makePublisher' ::
       Proxy (PublishingJSON s a)
    -> (backend -> Route -> Text -> IO ())
    -> (backend -> Streamly.Serial a -> IO ())
  makePublisher' _ publish =
    \backend req -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      Streamly.mapM_ (publish backend path . encodeToText) req

-- The Subscriber type families.
type family Subscriber backend spec :: *

type family Subscriber' backend spec :: *

type instance
     Subscriber backend (PublishingAs (f :: Format) (s :: Symbol) a) =
     backend -> IO a

type instance
     Subscriber' backend (PublishingAs (f :: Format) (s :: Symbol) a) =
     backend -> IO (Id, Streamly.Serial a)

type instance Subscriber backend (a :<|> b) =
     Subscriber backend a :<|> Subscriber backend b

type instance Subscriber' backend (a :<|> b) =
     Subscriber' backend a :<|> Subscriber' backend b

-- Used to generate client functions for an API (direct and streaming versions).
-- Param is function of route that returns (subscriber ID, result or result stream).
class HasSubscriber backend spec where
  makeSubscriber ::
       Proxy spec -> (backend -> Route -> IO Text) -> Subscriber backend spec
  makeSubscriber' ::
       Proxy spec
    -> (backend -> Route -> IO (Id, Streamly.Serial Text))
    -> Subscriber' backend spec

instance (HasSubscriber backend a, HasSubscriber backend b) =>
         HasSubscriber backend (a :<|> b) where
  makeSubscriber ::
       Proxy (a :<|> b)
    -> (backend -> Route -> IO Text)
    -> Subscriber backend a :<|> Subscriber backend b
  makeSubscriber _ receive =
    makeSubscriber (Proxy :: Proxy a) receive :<|>
    makeSubscriber (Proxy :: Proxy b) receive
  makeSubscriber' ::
       Proxy (a :<|> b)
    -> (backend -> Route -> IO (Id, Streamly.Serial Text))
    -> Subscriber' backend a :<|> Subscriber' backend b
  makeSubscriber' _ receive =
    makeSubscriber' (Proxy :: Proxy a) receive :<|>
    makeSubscriber' (Proxy :: Proxy b) receive

instance (KnownSymbol s, Read a) =>
         HasSubscriber backend (Publishing (s :: Symbol) a) where
  makeSubscriber ::
       Proxy (Publishing s a)
    -> (backend -> Route -> IO Text)
    -> (backend -> IO a)
  makeSubscriber _ receiveIO =
    \backend -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      result <- receiveIO backend path
      res <- readUnsafe result
      return res
  makeSubscriber' ::
       Proxy (Publishing s a)
    -> (backend -> Route -> IO (Id, Streamly.Serial Text))
    -> (backend -> IO (Id, Streamly.Serial a))
  makeSubscriber' _ receiveIO =
    \backend -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      (id, results) <- receiveIO backend path
      return $ (id, Streamly.mapM readUnsafe results)

instance (KnownSymbol s, FromJSON a) =>
         HasSubscriber backend (PublishingJSON (s :: Symbol) a) where
  makeSubscriber ::
       Proxy (PublishingJSON s a)
    -> (backend -> Route -> IO Text)
    -> (backend -> IO a)
  makeSubscriber _ receiveIO =
    \backend -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      result <- receiveIO backend path
      res <- decodeUnsafe result
      return res
  makeSubscriber' ::
       Proxy (PublishingJSON s a)
    -> (backend -> Route -> IO (Id, Streamly.Serial Text))
    -> (backend -> IO (Id, Streamly.Serial a))
  makeSubscriber' _ receiveIO =
    \backend -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      (id, results) <- receiveIO backend path
      return $ (id, Streamly.mapM decodeUnsafe results)
