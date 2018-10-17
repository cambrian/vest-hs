module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.Text (breakOn)
import Data.Typeable (tyConModule, typeRepTyCon)
import Vest.Prelude.Core

type Namespace = Text' "Namespace"

type NamespacedText' t = Tagged '( "Namespaced", t) Text

getNamespace :: NamespacedText' t -> Namespace
getNamespace = Tagged . fst . breakOn "/" . untag

class HasNamespace a where
  namespace :: Namespace
  namespaced' :: Text' t -> NamespacedText' t
  namespaced' text = retag $ retag (namespace @a) <> "/" <> text

moduleName' ::
     forall a t. Typeable a
  => Text' t
moduleName' =
  Tagged . pack . tyConModule . typeRepTyCon $ typeRep (Proxy :: Proxy a)
