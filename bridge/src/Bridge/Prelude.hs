module Bridge.Prelude
  ( module Bridge.Prelude
  ) where

import VestPrelude

data Format
  = Haskell
  | JSON
  deriving (Eq, Ord, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

deserializeOf :: (Read a, FromJSON a) => Format -> (Text -> Maybe a)
deserializeOf Haskell = read
deserializeOf JSON = decode

deserializeUnsafeOf :: (Read a, FromJSON a) => Format -> (Text -> IO a)
deserializeUnsafeOf Haskell = readUnsafe
deserializeUnsafeOf JSON = decodeUnsafe

serializeOf :: (Show a, ToJSON a) => Format -> (a -> Text)
serializeOf Haskell = show
serializeOf JSON = encode
