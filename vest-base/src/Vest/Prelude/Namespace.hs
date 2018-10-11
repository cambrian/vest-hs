module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.Typeable (tyConModule, typeRepTyCon)
import Vest.Prelude.Core

type NamespacedText' t = Tagged '( "Namespaced", t) Text

class HasNamespace a where
  namespace :: Text' "Namespace"
  namespaced' :: Text' t -> NamespacedText' t
  namespaced' text = retag $ retag (namespace @a) <> "/" <> text

moduleName' ::
     forall a t. Typeable a
  => Text' t
moduleName' =
  Tagged . pack . tyConModule . typeRepTyCon $ typeRep (Proxy :: Proxy a)
