module Vest.Prelude.Serialize
  ( module Vest.Prelude.Serialize
  ) where

import Data.Aeson
import GHC.Base (String)
import Vest.Prelude.Core
import Vest.Prelude.TypeLevel (symbolText)

data SerializationFormat
  = Haskell
  | JSON
  | Pretty
  deriving (Eq, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

newtype DeserializeException (fmt :: SerializationFormat) =
  DeserializeException Text
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Exception, Hashable, FromJSON, ToJSON)

class Serializable (fmt :: SerializationFormat) a where
  serialize :: a -> Text
  serialize' :: a -> Text' t
  serialize' = Tagged . serialize @fmt

instance Show a => Serializable 'Haskell a where
  serialize = show

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

show' :: (Show a) => a -> Text' t
show' = serialize' @'Haskell

-- Pretty printing. This might be good to break out into a submodule.
--
-- Summary of differences from read/show for base types:
-- * Strings (and string-like types) serialized without surrounding quotes.
-- * Tagged serialized without (Tagged ...) wrapping.
-- * (KnownSymbol s) => Proxy s serialized as s, instead of Proxy.
--
-- Add instances as needed.
instance {-# OVERLAPPABLE #-} Show a => Serializable 'Pretty a where
  serialize = show

instance {-# OVERLAPPABLE #-} Read a => Deserializable 'Pretty a where
  deserialize = read

-- Gives duplicate instance error since the right side is identical to the above:
-- TODO: add phantom type param to Serializable/Deserializable to get around this?
-- instance {-# OVERLAPS #-} ConvertibleStrings a Text =>
--                           Serializable 'Pretty a where
--   serialize = convertString
-- instance {-# OVERLAPS #-} ConvertibleStrings Text a =>
--                           Deserializable 'Pretty a where
--   deserialize = Just . convertString
instance {-# OVERLAPPING #-} Serializable 'Pretty String where
  serialize = convertString

instance {-# OVERLAPPING #-} Deserializable 'Pretty String where
  deserialize = Just . convertString

instance {-# OVERLAPPING #-} Serializable 'Pretty Text where
  serialize = convertString

instance {-# OVERLAPPING #-} Deserializable 'Pretty Text where
  deserialize = Just . convertString

instance {-# OVERLAPPING #-} Serializable 'Pretty LazyText where
  serialize = convertString

instance {-# OVERLAPPING #-} Deserializable 'Pretty LazyText where
  deserialize = Just . convertString

instance {-# OVERLAPPING #-} Serializable 'Pretty ByteString where
  serialize = convertString

instance {-# OVERLAPPING #-} Deserializable 'Pretty ByteString where
  deserialize = Just . convertString

instance {-# OVERLAPPING #-} Serializable 'Pretty LazyByteString where
  serialize = convertString

instance {-# OVERLAPPING #-} Deserializable 'Pretty LazyByteString where
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
