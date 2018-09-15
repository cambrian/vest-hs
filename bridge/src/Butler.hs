module Butler where

import qualified Data.Text as Text
import GHC.TypeLits
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

-- Parameters: API and a function to send outgoing values.
-- Returns: Function that takes a Route and a Request to serve.
-- If an invalid handler is given, this will silently swallow the request.
serve ::
     (HasServer spec)
  => Proxy spec
  -> API spec
  -> (Text -> IO ())
  -> (Route -> Text -> IO ())
serve proxy api send path x =
  case route proxy api path x of
    Nothing -> return () -- Instead: Exception?
    Just server -> do
      result <- server
      send result

serve' ::
     (HasServer spec)
  => Proxy spec
  -> API' spec
  -> (Streamly.Serial Text -> IO ())
  -> (Route -> Text -> IO ())
serve' proxy api send path x =
  case route' proxy api path x of
    Nothing -> return () -- Instead: Exception?
    Just server -> do
      result <- server
      send result

-- API specification DSL.
data Request (a :: *)

data Response (a :: *)

data Publish (a :: *)

data a :<|> b =
  a :<|> b

infixr 8 :<|>

data Format
  = Haskell
  | JSON

data ProtocolAs (f :: Format) (s :: k) (a :: *) (b :: *)

type Protocol = ProtocolAs 'Haskell

type ProtocolJSON = ProtocolAs 'JSON

-- The API type families.
type family API spec :: *

type family API' spec :: *

type instance
     API
       (ProtocolAs (f :: Format) (s :: Symbol) (Request a) (Response b))
     = a -> IO b

type instance
     API'
       (ProtocolAs (f :: Format) (s :: Symbol) (Request a) (Response b))
     = a -> IO (Streamly.Serial b)

type instance API (a :<|> b) = API a :<|> API b

type instance API' (a :<|> b) = API' a :<|> API' b

-- Used to route requests to the right handler.
-- Apostrophe versions are streaming, and direct routes must be served separately from streaming
-- routes (for clarity in implementation).
class HasServer spec
  -- Params are route path and request.
  where
  route :: Proxy spec -> API spec -> Route -> Text -> Maybe (IO Text)
  route' ::
       Proxy spec
    -> API' spec
    -> Route
    -> Text
    -> Maybe (IO (Streamly.Serial Text))

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route ::
       Proxy (a :<|> b)
    -> (API a :<|> API b)
    -> Route
    -> Text
    -> Maybe (IO Text)
  route _ (handlerA :<|> handlerB) path request =
    route (Proxy :: Proxy a) handlerA path request <|>
    route (Proxy :: Proxy b) handlerB path request
  route' ::
       Proxy (a :<|> b)
    -> (API' a :<|> API' b)
    -> Route
    -> Text
    -> Maybe (IO (Streamly.Serial Text))
  route' _ (handlerA :<|> handlerB) path request =
    route' (Proxy :: Proxy a) handlerA path request <|>
    route' (Proxy :: Proxy b) handlerB path request

instance (KnownSymbol s, Read a, Show b) =>
         HasServer (Protocol (s :: Symbol) (Request a) (Response b)) where
  route ::
       Proxy (Protocol s (Request a) (Response b))
    -> (a -> IO b)
    -> Route
    -> Text
    -> Maybe (IO Text)
  route _ handler (Route _path) request
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ do
        req <- readUnsafe request
        result <- handler req
        return $ show result
  route _ _ _ _ = Nothing
  route' ::
       Proxy (Protocol s (Request a) (Response b))
    -> (a -> IO (Streamly.Serial b))
    -> Route
    -> Text
    -> Maybe (IO (Streamly.Serial Text))
  route' _ handler (Route _path) request
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ do
        req <- readUnsafe request
        results <- handler req
        return $ Streamly.map show results
  route' _ _ _ _ = Nothing

instance (KnownSymbol s, FromJSON a, ToJSON b) =>
         HasServer (ProtocolJSON (s :: Symbol) (Request a) (Response b)) where
  route ::
       Proxy (ProtocolJSON s (Request a) (Response b))
    -> (a -> IO b)
    -> Route
    -> Text
    -> Maybe (IO Text)
  route _ handler (Route _path) request
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ do
        req <- decodeUnsafe request
        result <- handler req
        return $ encodeToText result
  route _ _ _ _ = Nothing
  route' ::
       Proxy (ProtocolJSON s (Request a) (Response b))
    -> (a -> IO (Streamly.Serial b))
    -> Route
    -> Text
    -> Maybe (IO (Streamly.Serial Text))
  route' _ handler (Route _path) request
    | symbolVal (Proxy :: Proxy s) == Text.unpack _path =
      Just $ do
        req <- decodeUnsafe request
        results <- handler req
        return $ Streamly.map encodeToText results
  route' _ _ _ _ = Nothing

-- The Client type family.
type family Client spec :: *

type family Client' spec :: *

type instance
     Client
       (ProtocolAs (f :: Format) (s :: Symbol) (Request a) (Response b))
     = a -> IO b

type instance
     Client'
       (ProtocolAs (f :: Format) (s :: Symbol) (Request a) (Response b))
     = a -> IO (Streamly.Serial b)

type instance Client (a :<|> b) = Client a :<|> Client b

type instance Client' (a :<|> b) = Client' a :<|> Client' b

-- Used to generate client functions for an API (direct and streaming versions).
class HasClient spec where
  makeClient :: Proxy spec -> (Text -> IO ()) -> IO Text -> Client spec
  makeClient' :: Proxy spec -> (Text -> IO ()) -> IO Text -> Client' spec-- Params are request publisher and acquired result.
-- instance (HasClient a, HasClient b) => HasServer (a :<|> b)
