module Bridge.Prelude
  ( module Bridge.Prelude
  ) where

import VestPrelude

type family Claims auth = claims | claims -> auth

class Auth t where
  verify :: Headers -> Id "RequestText" -> IO (Maybe (Claims t))

type instance Claims () = ()

verifyEmpty :: Headers -> Id "RequestText" -> IO (Maybe ())
verifyEmpty _ _ = return (Just ())

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
deserializeOf Haskell = read
deserializeOf JSON = decode

deserializeUnsafeOf :: (Read a, FromJSON a) => Format -> (Text -> IO a)
deserializeUnsafeOf Haskell = readUnsafe
deserializeUnsafeOf JSON = decodeUnsafe

serializeOf :: (Show a, ToJSON a) => Format -> (a -> Text)
serializeOf Haskell = show
serializeOf JSON = encode
