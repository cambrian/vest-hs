module Vest.Prelude.Serialize
  ( module Vest.Prelude.Serialize
  ) where

import Data.Aeson
import GHC.Base (String)
import Vest.Prelude.Core hiding (show) -- we redefine a non-polymorphic show in this module
import qualified Vest.Prelude.Core
import Vest.Prelude.TypeLevel (symbolText)

data SerializationFormat
  = Haskell
  | JSON
  | Pretty
  deriving (Eq, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

newtype DeserializeException (fmt :: SerializationFormat) =
  DeserializeException Text
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Exception, Hashable, ToJSON, FromJSON)

class Serializable (fmt :: SerializationFormat) a where
  serialize :: a -> Text
  serialize' :: a -> Text' t
  serialize' = Tagged . serialize @fmt

instance Show a => Serializable 'Haskell a where
  serialize = Vest.Prelude.Core.show

instance ToJSON a => Serializable 'JSON a where
  serialize = convertString . encode

class (Typeable fmt) =>
      Deserializable (fmt :: SerializationFormat) a
  where
  deserialize :: Text -> Maybe a
  deserialize' :: Text' t -> Maybe a
  deserialize' = deserialize @fmt . untag
  deserializeUnsafe :: Text -> IO a
  deserializeUnsafe text =
    fromJustUnsafe (DeserializeException @fmt text) $ deserialize @fmt text
  deserializeUnsafe' :: Text' t -> IO a
  deserializeUnsafe' = deserializeUnsafe @fmt . untag

instance Read a => Deserializable 'Haskell a where
  deserialize = readMaybe . convertString

instance FromJSON a => Deserializable 'JSON a where
  deserialize = decode . convertString

-- Shorthands for read/show.
-- Not providing encode/decode; prefer serialize/deserialize @'JSON
read :: (Read a) => Text -> Maybe a
read = deserialize @'Haskell

read' :: (Read a) => Text' t -> Maybe a
read' = deserialize' @'Haskell

readUnsafe :: (Read a) => Text -> IO a
readUnsafe = deserializeUnsafe @'Haskell

readUnsafe' :: (Read a) => Text' t -> IO a
readUnsafe' = deserializeUnsafe' @'Haskell

show :: (Show a) => a -> Text
-- ^ Redefine show to be text only.
-- If you want the old polymorphic behavior, use convertString . show
show = serialize @'Haskell

show' :: (Show a) => a -> Text' t
show' = serialize' @'Haskell

-- Pretty printing. This might be good to break out into a submodule.
--
-- Currently prints strings without the surrounding quotations.
-- Add instances as needed
instance {-# OVERLAPPABLE #-} Show a => Serializable 'Pretty a where
  serialize = show

instance {-# OVERLAPPABLE #-} Read a => Deserializable 'Pretty a where
  deserialize = read

-- Gives duplicate instance error for some reason
-- instance {-# OVERLAPS #-} ConvertibleStrings a Text =>
--                           Serializable 'Pretty a where
--   serialize = convertString
-- instance {-# OVERLAPS #-} ConvertibleStrings Text a =>
--                           Deserializable 'Pretty a where
--   deserialize = Just . convertString
instance {-# OVERLAPS #-} Serializable 'Pretty String where
  serialize = convertString

instance {-# OVERLAPS #-} Deserializable 'Pretty String where
  deserialize = Just . convertString

instance {-# OVERLAPS #-} Serializable 'Pretty Text where
  serialize = convertString

instance {-# OVERLAPS #-} Deserializable 'Pretty Text where
  deserialize = Just . convertString

instance {-# OVERLAPS #-} Serializable 'Pretty LazyText where
  serialize = convertString

instance {-# OVERLAPS #-} Deserializable 'Pretty LazyText where
  deserialize = Just . convertString

instance {-# OVERLAPS #-} Serializable 'Pretty ByteString where
  serialize = convertString

instance {-# OVERLAPS #-} Deserializable 'Pretty ByteString where
  deserialize = Just . convertString

instance {-# OVERLAPS #-} Serializable 'Pretty LazyByteString where
  serialize = convertString

instance {-# OVERLAPS #-} Deserializable 'Pretty LazyByteString where
  deserialize = Just . convertString

instance {-# OVERLAPPING #-} Serializable 'Pretty a =>
                             Serializable 'Pretty (Tagged s a) where
  serialize = serialize @'Pretty . untag

instance {-# OVERLAPPING #-} Deserializable 'Pretty a =>
                             Deserializable 'Pretty (Tagged s a) where
  deserialize x = deserialize @'Pretty x >>- Tagged

instance {-# OVERLAPPING #-} KnownSymbol a =>
                             Serializable 'Pretty (Proxy a) where
  serialize _ = symbolText (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} KnownSymbol a =>
                             Deserializable 'Pretty (Proxy a) where
  deserialize a =
    if a == symbolText (Proxy :: Proxy a)
      then Just Proxy
      else Nothing
