module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.List (span)
import Data.Text (takeWhile)
import Vest.Prelude.Core hiding (takeWhile)

-- TODO: Make doctests for pretty serialization roundtrip, isstring.
newtype Namespaced (ns :: k) a =
  Namespaced (Text' ns, a)
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

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
  default namespace :: Typeable t =>
    Text
  namespace = takeWhile (/= '.') $ moduleName @t
  -- ^ Turns DummyManager.Internal (for example) into DummyManager. We can reconsider this in the
  -- future if we ever need multiple namespaces within the same root module name.
  namespace' :: forall ns. Text' ns
  namespace' = Tagged $ namespace @t
  namespaced :: forall ns a. a -> Namespaced ns a
  namespaced a = Namespaced (namespace' @t @ns, a)
