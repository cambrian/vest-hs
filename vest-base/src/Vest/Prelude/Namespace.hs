module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.List (span)
import Vest.Prelude.Core

-- TODO: doctests for pretty serialization roundtrip, isstring
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
  namespace' :: forall ns. Text' ns
  namespace' = Tagged $ namespace @t
  namespaced :: forall ns a. a -> Namespaced ns a
  namespaced a = Namespaced (namespace' @t @ns, a)
