module Vest.Prelude.Serialize
  ( module Vest.Prelude.Serialize
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LazyByteString
import qualified Text.Read
import Vest.Prelude.Core

newtype DeserializeException =
  DeserializeException (Text' "Format", Text)
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Exception, Hashable, ToJSON, FromJSON)

class SerializationFormat fmt where
  deserializeException :: Text -> DeserializeException

instance SerializationFormat "Haskell" where
  deserializeException = DeserializeException . (Tagged "Haskell", )

instance SerializationFormat "JSON" where
  deserializeException = DeserializeException . (Tagged "JSON", )

class (SerializationFormat fmt) =>
      Serializable fmt a
  where
  serialize :: a -> Text
  serialize' :: a -> Text' t
  serialize' = Tagged . serialize @fmt

instance Show a => Serializable "Haskell" a where
  serialize = show

instance ToJSON a => Serializable "JSON" a where
  serialize = pack . LazyByteString.toString . encode

class (SerializationFormat fmt) =>
      Deserializable fmt a
  where
  deserialize :: Text -> Maybe a
  deserialize' :: Text' t -> Maybe a
  deserialize' = deserialize @fmt . untag
  deserializeUnsafe :: Text -> IO a
  deserializeUnsafe text =
    fromJustUnsafe (deserializeException @fmt text) $ deserialize @fmt text
  deserializeUnsafe' :: Text' t -> IO a
  deserializeUnsafe' = deserializeUnsafe @fmt . untag

instance Read a => Deserializable "Haskell" a where
  deserialize = Text.Read.readMaybe . unpack

instance FromJSON a => Deserializable "JSON" a where
  deserialize = decode . LazyByteString.fromString . unpack

read :: (Read a) => Text -> Maybe a
read = deserialize @"Haskell"

read' :: (Read a) => Text' t -> Maybe a
read' = deserialize' @"Haskell"

show' :: (Show a) => a -> Text' t
show' = serialize' @"Haskell"
-- show is defined in protolude
-- Not providing encode/decode because you should prefer serialize/deserialize @'JSON.
