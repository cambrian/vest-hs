module Butler
  ( Format(..)
  , Server
  , Server'
  , Protocol
  , Publishing
  , ProtocolJSON
  , PublishingJSON
  , (:<|>)(..)
  , makeRouter
  , makeRouter'
  , makeClient
  , makeClient'
  , makePublish
  , makePublish'
  , makeSubscribe
  , makeSubscribe'
  ) where

import qualified Data.Text as Text
import GHC.TypeLits
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

symbolToRoute :: (KnownSymbol a) => Proxy a -> Route
symbolToRoute = Route . Text.pack . symbolVal

-- This library can also throw ReadExceptions and DecodeExceptions.
newtype ButlerException =
  NoSuchRoute Text
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

makeRouter ::
     (HasServer spec)
  => Proxy spec
  -> Server spec
  -> (Route -> IO (Text -> IO Text))
makeRouter proxy server =
  \path ->
    case route proxy server path of
      Nothing -> do
        let Route _path = path
        throw $ NoSuchRoute _path
      Just handler -> return handler

makeRouter' ::
     (HasServer spec)
  => Proxy spec
  -> Server' spec
  -> (Route -> IO (Text -> IO (Streamly.Serial Text)))
makeRouter' proxy server =
  \path ->
    case route' proxy server path of
      Nothing -> do
        let Route _path = path
        throw $ NoSuchRoute _path
      Just handler -> return handler

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
-- routes (for clarity in implementation).
-- Params are route path and request.
class HasServer spec where
  route :: Proxy spec -> Server spec -> Route -> Maybe (Text -> IO Text)
  route' ::
       Proxy spec
    -> Server' spec
    -> Route
    -> Maybe (Text -> IO (Streamly.Serial Text))

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route ::
       Proxy (a :<|> b)
    -> (Server a :<|> Server b)
    -> Route
    -> Maybe (Text -> IO Text)
  route _ (handlerA :<|> handlerB) path =
    route (Proxy :: Proxy a) handlerA path <|>
    route (Proxy :: Proxy b) handlerB path
  route' ::
       Proxy (a :<|> b)
    -> (Server' a :<|> Server' b)
    -> Route
    -> Maybe (Text -> IO (Streamly.Serial Text))
  route' _ (handlerA :<|> handlerB) path =
    route' (Proxy :: Proxy a) handlerA path <|>
    route' (Proxy :: Proxy b) handlerB path

instance (KnownSymbol s, Read a, Show b) =>
         HasServer (Protocol (s :: Symbol) a b) where
  route ::
       Proxy (Protocol s a b) -> (a -> IO b) -> Route -> Maybe (Text -> IO Text)
  route _ handler (Route _path)
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ \request -> do
        req <- readUnsafe request
        result <- handler req
        return $ show result
  route _ _ _ = Nothing
  route' ::
       Proxy (Protocol s a b)
    -> (a -> IO (Streamly.Serial b))
    -> Route
    -> Maybe (Text -> IO (Streamly.Serial Text))
  route' _ handler (Route _path)
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ \request -> do
        req <- readUnsafe request
        results <- handler req
        return $ Streamly.map show results
  route' _ _ _ = Nothing

instance (KnownSymbol s, FromJSON a, ToJSON b) =>
         HasServer (ProtocolJSON (s :: Symbol) a b) where
  route ::
       Proxy (ProtocolJSON s a b)
    -> (a -> IO b)
    -> Route
    -> Maybe (Text -> IO Text)
  route _ handler (Route _path)
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ \request -> do
        req <- decodeUnsafe request
        result <- handler req
        return $ encodeToText result
  route _ _ _ = Nothing
  route' ::
       Proxy (ProtocolJSON s a b)
    -> (a -> IO (Streamly.Serial b))
    -> Route
    -> Maybe (Text -> IO (Streamly.Serial Text))
  route' _ handler (Route _path)
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ \request -> do
        req <- decodeUnsafe request
        results <- handler req
        return $ Streamly.map encodeToText results
  route' _ _ _ = Nothing

-- The Client type families.
type family Client spec :: *

type family Client' spec :: *

type instance Client (ProtocolAs (f :: Format) (s :: Symbol) a b) =
     a -> IO b

type instance Client' (ProtocolAs (f :: Format) (s :: Symbol) a b)
     = a -> IO (Streamly.Serial b)

type instance Client (a :<|> b) = Client a :<|> Client b

type instance Client' (a :<|> b) = Client' a :<|> Client' b

-- Used to generate client functions for an API (direct and streaming versions).
-- Param is request publisher.
class HasClient spec where
  makeClient :: Proxy spec -> (Route -> Text -> IO Text) -> Client spec
  makeClient' ::
       Proxy spec
    -> (Route -> Text -> IO (Streamly.Serial Text))
    -> Client' spec

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  makeClient ::
       Proxy (a :<|> b) -> (Route -> Text -> IO Text) -> Client a :<|> Client b
  makeClient _ publish =
    makeClient (Proxy :: Proxy a) publish :<|>
    makeClient (Proxy :: Proxy b) publish
  makeClient' ::
       Proxy (a :<|> b)
    -> (Route -> Text -> IO (Streamly.Serial Text))
    -> Client' a :<|> Client' b
  makeClient' _ publish =
    makeClient' (Proxy :: Proxy a) publish :<|>
    makeClient' (Proxy :: Proxy b) publish

instance (KnownSymbol s, Show a, Read b) =>
         HasClient (Protocol (s :: Symbol) a b) where
  makeClient ::
       Proxy (Protocol s a b) -> (Route -> Text -> IO Text) -> (a -> IO b)
  makeClient _ publish =
    \req -> do
      let path = symbolToRoute (Proxy :: Proxy s)
      result <- publish path (show req)
      readUnsafe result
  makeClient' ::
       Proxy (Protocol s a b)
    -> (Route -> Text -> IO (Streamly.Serial Text))
    -> (a -> IO (Streamly.Serial b))
  makeClient' _ publish =
    \req -> do
      let path = symbolToRoute (Proxy :: Proxy s)
      results <- publish path (show req)
      return $ Streamly.mapM readUnsafe results

instance (KnownSymbol s, ToJSON a, FromJSON b) =>
         HasClient (ProtocolJSON (s :: Symbol) a b) where
  makeClient ::
       Proxy (ProtocolJSON s a b) -> (Route -> Text -> IO Text) -> (a -> IO b)
  makeClient _ publish =
    \req -> do
      let path = symbolToRoute (Proxy :: Proxy s)
      result <- publish path (encodeToText req)
      decodeUnsafe result
  makeClient' ::
       Proxy (ProtocolJSON s a b)
    -> (Route -> Text -> IO (Streamly.Serial Text))
    -> (a -> IO (Streamly.Serial b))
  makeClient' _ publish =
    \req -> do
      let path = (symbolToRoute (Proxy :: Proxy s))
      results <- publish path (encodeToText req)
      return $ Streamly.mapM decodeUnsafe results

-- The Publisher type families.
type family Publisher spec :: *

type family Publisher' spec :: *

type instance
     Publisher (PublishingAs (f :: Format) (s :: Symbol) a) = a -> IO ()

type instance
     Publisher' (PublishingAs (f :: Format) (s :: Symbol) a) =
     (Streamly.Serial a) -> IO ()

type instance Publisher (a :<|> b) = Publisher a :<|> Publisher b

type instance Publisher' (a :<|> b) =
     Publisher' a :<|> Publisher' b

-- Used to generate publish functions for an API (direct and streaming versions).
-- Param is request publisher.
class HasPublisher spec where
  makePublish :: Proxy spec -> (Route -> Text -> IO ()) -> Publisher spec
  makePublish' :: Proxy spec -> (Route -> Text -> IO ()) -> Publisher' spec

instance (HasPublisher a, HasPublisher b) => HasPublisher (a :<|> b) where
  makePublish ::
       Proxy (a :<|> b)
    -> (Route -> Text -> IO ())
    -> Publisher a :<|> Publisher b
  makePublish _ publish =
    makePublish (Proxy :: Proxy a) publish :<|>
    makePublish (Proxy :: Proxy b) publish
  makePublish' ::
       Proxy (a :<|> b)
    -> (Route -> Text -> IO ())
    -> Publisher' a :<|> Publisher' b
  makePublish' _ publish =
    makePublish' (Proxy :: Proxy a) publish :<|>
    makePublish' (Proxy :: Proxy b) publish

instance (KnownSymbol s, Show a) =>
         HasPublisher (Publishing (s :: Symbol) a) where
  makePublish ::
       Proxy (Publishing s a) -> (Route -> Text -> IO ()) -> (a -> IO ())
  makePublish _ publish =
    let path = (symbolToRoute (Proxy :: Proxy s))
     in publish path . show
  makePublish' ::
       Proxy (Publishing s a)
    -> (Route -> Text -> IO ())
    -> ((Streamly.Serial a) -> IO ())
  makePublish' _ publish =
    let path = (symbolToRoute (Proxy :: Proxy s))
     in Streamly.mapM_ (publish path . show)

instance (KnownSymbol s, ToJSON a) =>
         HasPublisher (PublishingJSON (s :: Symbol) a) where
  makePublish ::
       Proxy (PublishingJSON s a) -> (Route -> Text -> IO ()) -> (a -> IO ())
  makePublish _ publish =
    let path = (symbolToRoute (Proxy :: Proxy s))
     in publish path . encodeToText
  makePublish' ::
       Proxy (PublishingJSON s a)
    -> (Route -> Text -> IO ())
    -> ((Streamly.Serial a) -> IO ())
  makePublish' _ publish =
    let path = (symbolToRoute (Proxy :: Proxy s))
     in Streamly.mapM_ (publish path . encodeToText)

-- The Subscriber type families.
type family Subscriber spec :: *

type family Subscriber' spec :: *

type instance
     Subscriber (PublishingAs (f :: Format) (s :: Symbol) a) = IO a

type instance
     Subscriber' (PublishingAs (f :: Format) (s :: Symbol) a) =
     IO (Id, Streamly.Serial a)

type instance Subscriber (a :<|> b) =
     Subscriber a :<|> Subscriber b

type instance Subscriber' (a :<|> b) =
     Subscriber' a :<|> Subscriber' b

-- Used to generate client functions for an API (direct and streaming versions).
-- Param is function of route that returns (subscriber ID, result or result stream).
class HasSubscriber spec where
  makeSubscribe :: Proxy spec -> (Route -> IO Text) -> Subscriber spec
  makeSubscribe' ::
       Proxy spec
    -> (Route -> IO (Id, Streamly.Serial Text))
    -> Subscriber' spec

instance (HasSubscriber a, HasSubscriber b) => HasSubscriber (a :<|> b) where
  makeSubscribe ::
       Proxy (a :<|> b) -> (Route -> IO Text) -> Subscriber a :<|> Subscriber b
  makeSubscribe _ receive =
    makeSubscribe (Proxy :: Proxy a) receive :<|>
    makeSubscribe (Proxy :: Proxy b) receive
  makeSubscribe' ::
       Proxy (a :<|> b)
    -> (Route -> IO (Id, Streamly.Serial Text))
    -> Subscriber' a :<|> Subscriber' b
  makeSubscribe' _ receive =
    makeSubscribe' (Proxy :: Proxy a) receive :<|>
    makeSubscribe' (Proxy :: Proxy b) receive

instance (KnownSymbol s, Read a) =>
         HasSubscriber (Publishing (s :: Symbol) a) where
  makeSubscribe :: Proxy (Publishing s a) -> (Route -> IO Text) -> IO a
  makeSubscribe _ receiveIO = do
    let path = (symbolToRoute (Proxy :: Proxy s))
    result <- receiveIO path
    res <- readUnsafe result
    return res
  makeSubscribe' ::
       Proxy (Publishing s a)
    -> (Route -> IO (Id, Streamly.Serial Text))
    -> IO (Id, Streamly.Serial a)
  makeSubscribe' _ receiveIO = do
    let path = (symbolToRoute (Proxy :: Proxy s))
    (id, results) <- receiveIO path
    return $ (id, Streamly.mapM readUnsafe results)

instance (KnownSymbol s, FromJSON a) =>
         HasSubscriber (PublishingJSON (s :: Symbol) a) where
  makeSubscribe :: Proxy (PublishingJSON s a) -> (Route -> IO Text) -> IO a
  makeSubscribe _ receiveIO = do
    let path = (symbolToRoute (Proxy :: Proxy s))
    result <- receiveIO path
    res <- decodeUnsafe result
    return res
  makeSubscribe' ::
       Proxy (PublishingJSON s a)
    -> (Route -> IO (Id, Streamly.Serial Text))
    -> IO (Id, Streamly.Serial a)
  makeSubscribe' _ receiveIO = do
    let path = (symbolToRoute (Proxy :: Proxy s))
    (id, results) <- receiveIO path
    return $ (id, Streamly.mapM decodeUnsafe results)
