module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.List (span)
import Data.Text (breakOn, stripPrefix)
import System.IO.Unsafe (unsafePerformIO)
import Vest.Prelude.Core
import Vest.Prelude.Serialize
import Vest.Prelude.TypeLevel (symbolText)

-- TODO: Make doctests for pretty serialization roundtrip, isstring.
newtype Namespaced (ns :: k) a =
  Namespaced (Text' ns, a)
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance (IsString a) => IsString (Namespaced ns a) where
  fromString s =
    let (ns, slasha) = span (/= '/') s
     in Namespaced (Tagged $ pack ns, fromString $ tailSafe slasha)

instance (Serializable 'Pretty a) =>
         Serializable 'Pretty (Namespaced ns a) where
  serialize (Namespaced (ns, a)) =
    serialize @'Pretty ns <> "/" <> serialize @'Pretty a

instance (Deserializable 'Pretty a) =>
         Deserializable 'Pretty (Namespaced ns a) where
  deserialize text = do
    let (ns, slasha) = breakOn "/" text
    a <- stripPrefix "/" slasha >>= deserialize @'Pretty
    return $ Namespaced (Tagged ns, a)

getNamespace' :: Namespaced ns a -> Text' ns
getNamespace' (Namespaced (ns, _)) = ns

unnamespaced :: Namespaced ns a -> a
unnamespaced (Namespaced (_, a)) = a

-- | TODO: type level check that Namespace t is a valid rel dir. Possibly impossible without
-- dependent typing?
class KnownSymbol (Namespace t) =>
      HasNamespace t
  where
  type Namespace t :: Symbol
  -- type Namespace t = ModuleName t
  -- ^ TODO: why doesn't this work?
  namespace :: Text
  namespace = symbolText (Proxy :: Proxy (Namespace t))
  namespaceDir :: Path Rel Dir
  namespaceDir =
    unsafePerformIO $ parseRelDir $ symbolVal (Proxy :: Proxy (Namespace t))
  namespaced :: forall ns a. a -> Namespaced ns a
  namespaced a = Namespaced (Tagged $ namespace @t, a) -- ^ TODO: automatic instances for generic/typeable/whatever happens to work via
-- deriving (HasDefaultNamespace)
-- or similar
