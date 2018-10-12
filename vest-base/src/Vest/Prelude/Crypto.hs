module Vest.Prelude.Crypto
  ( module Reexports
  , SignedText'
  , seedKeyPairUnsafe
  , sign'
  , verify'
  ) where

import Crypto.Sign.Ed25519 as Reexports hiding (sign, sign', verify, verify')
import Vest.Prelude.Core

data SeedNot32BytesException =
  SeedNot32BytesException
  deriving (Show, Exception)

instance Hashable PublicKey

deriving instance Read PublicKey

instance ToJSON PublicKey

instance FromJSON PublicKey

instance ToJSONKey PublicKey

instance FromJSONKey PublicKey

deriving instance Read Signature

-- | We provide a signed type only for Texts, to avoid promoting extra serialization roundtrips
-- for verifying arbitrary types.
type SignedText' t = (Signature, Text' t)

seedKeyPairUnsafe :: ByteString -> IO (PublicKey, SecretKey)
-- ^ Throws if seed bytestring is not exactly 32 bytes.
seedKeyPairUnsafe =
  fromJustUnsafe SeedNot32BytesException . createKeypairFromSeed_

sign' :: SecretKey -> Text' t -> SignedText' t
sign' secret text' = (dsign secret (encodeUtf8 $ untag text'), text')

verify' :: PublicKey -> SignedText' t -> Maybe (Text' t)
verify' pubKey (sig, text') =
  if dverify pubKey (encodeUtf8 $ untag text') sig
    then Just text'
    else Nothing
