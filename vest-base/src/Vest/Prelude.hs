module Vest.Prelude
  ( module Reexports
  ) where

import Data.Text (breakOn, stripPrefix)
import Vest.Prelude.Blockchain as Reexports
import Vest.Prelude.Core as Reexports
import Vest.Prelude.Counter as Reexports
import Vest.Prelude.Crypto as Reexports
import Vest.Prelude.Money as Reexports
import Vest.Prelude.Namespace as Reexports
import Vest.Prelude.Resource as Reexports
import Vest.Prelude.Serialize as Reexports
import Vest.Prelude.Stream as Reexports
import Vest.Prelude.Time as Reexports
import Vest.Prelude.TypeLevel as Reexports
import Vest.Prelude.UUID as Reexports

type StreamResource buf a = (StreamWriter a, Stream buf a)

instance Bufferable buf a => Resource (StreamResource buf a) where
  type ResourceConfig (StreamResource buf a) = ()
  make () = newStream @buf @a
  cleanup (writer, _) = closeStream writer

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
