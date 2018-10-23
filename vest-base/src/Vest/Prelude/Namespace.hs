module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.List (span)
import Data.Text (breakOn, stripPrefix)
import Data.Typeable (tyConModule, typeRepTyCon)
import Vest.Prelude.Core hiding (moduleName)
import Vest.Prelude.Serialize

-- TODO: doctests for pretty serialization roundtrip, isstring
newtype Namespaced (ns :: k) a =
  Namespaced (Text' ns, a)
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

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

instance (IsString a) => IsString (Namespaced ns a) where
  fromString s =
    let (ns, slasha) = span (/= '/') s
     in Namespaced (Tagged $ pack ns, fromString $ tailSafe slasha)

getNamespace' :: Namespaced ns a -> Text' ns
getNamespace' (Namespaced (ns, _)) = ns

unnamespaced :: Namespaced ns a -> a
unnamespaced (Namespaced (_, a)) = a

class HasNamespace t where
  namespace :: Text
  namespace' :: forall ns. Text' ns
  namespace' = Tagged $ namespace @t
  namespaced :: forall ns a. a -> Namespaced ns a
  namespaced a = Namespaced (namespace' @t @ns, a)

moduleName ::
     forall t. Typeable t
  => Text
moduleName = pack . tyConModule . typeRepTyCon $ typeRep (Proxy :: Proxy t)
