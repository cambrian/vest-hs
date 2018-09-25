module Bridge.Prelude
  ( module Bridge.Prelude
  ) where

import VestPrelude

data Auth
  = OpenAuth
  | TokenAuth
  deriving (Eq, Ord, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

-- TODO: Fill me in.
data Claims = Claims
  {
  }

data Format
  = Haskell
  | JSON
  deriving (Eq, Ord, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

data Headers = Headers
  { format :: Format
  , token :: Maybe Text -- JWT.
  -- TODO: Signatures and such.
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

deserializeOf :: (Read a, FromJSON a) => Format -> (Text -> Maybe a)
deserializeOf =
  \case
    Haskell -> read
    JSON -> decode

deserializeUnsafeOf :: (Read a, FromJSON a) => Format -> (Text -> IO a)
deserializeUnsafeOf =
  \case
    Haskell -> readUnsafe
    JSON -> decodeUnsafe

serializeOf :: (Show a, ToJSON a) => Format -> (a -> Text)
serializeOf =
  \case
    Haskell -> show
    JSON -> encode
