module Vest.Prelude.Namespace
  ( module Vest.Prelude.Namespace
  ) where

import Data.List (span)
import Data.Typeable (tyConModule, typeRepTyCon)
import GHC.Read (Read(readPrec))
import qualified GHC.Show (Show(show))
import Text.ParserCombinators.ReadP (char, munch)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Vest.Prelude.Core

-- TODO: custom ToJSON, FromJSON instances?
-- TODO: doctests for read/show/isstring
newtype Namespaced (s :: k) t =
  Namespaced (Text' s, t)
  deriving (Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance (Read t) => Read (Namespaced s t) where
  readPrec = do
    ns <- ReadPrec.lift $ munch (/= '/')
    ReadPrec.lift $ char '/'
    t <- readPrec
    return $ Namespaced (Tagged $ pack ns, t)

instance (Show t) => Show (Namespaced s t) where
  show (Namespaced (Tagged s, t)) = unpack s <> "/" <> GHC.Show.show t

instance (IsString t) => IsString (Namespaced s t) where
  fromString s =
    let (ns, slasht) = span (/= '/') s
     in Namespaced (Tagged $ pack ns, fromString $ tailSafe slasht)

getNamespace :: Namespaced s t -> Text' s
getNamespace (Namespaced (ns, _)) = ns

unnamespaced :: Namespaced s t -> t
unnamespaced (Namespaced (_, t)) = t

class HasNamespace a where
  namespace :: Text' s
  namespaced :: t -> Namespaced s t
  namespaced t = Namespaced (namespace @a, t)

moduleName' ::
     forall a t. Typeable a
  => Text' t
moduleName' =
  Tagged . pack . tyConModule . typeRepTyCon $ typeRep (Proxy :: Proxy a)
