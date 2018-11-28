module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.Text (stripPrefix)
import System.IO.Unsafe (unsafePerformIO)
import Vest.Prelude.Core
import Vest.Prelude.Serialize
import Vest.Prelude.TypeLevel (symbolText)

-- TODO: Make doctests for pretty serialization roundtrip.
newtype Namespaced ns a = Namespaced
  { unnamespaced :: a
  } deriving (Eq, Read, Show, Generic) deriving anyclass ( Hashable
                                                         , ToJSON
                                                         , FromJSON
                                                         )

instance (HasNamespace ns, Serializable 'Pretty a) =>
         Serializable 'Pretty (Namespaced ns a) where
  serialize (Namespaced a) = namespace @ns <> "/" <> serialize @'Pretty a

instance (HasNamespace ns, Deserializable 'Pretty a) =>
         Deserializable 'Pretty (Namespaced ns a) where
  deserialize text = do
    aText <- stripPrefix (namespace @ns <> "/") text
    a <- deserialize @'Pretty aText
    return $ Namespaced a

-- | TODO: type level check that Namespace t is a valid rel dir. Possibly impossible without
-- dependent typing?
-- Would also be nice to be able to write deriving (HasModuleNamespace) or similar, but default
-- implementations for associated types are not allowed. Maybe this will change at some point?
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
