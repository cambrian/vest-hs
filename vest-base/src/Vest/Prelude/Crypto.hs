-- | Crypto primitives
-- Uses ed25519 for public key cryptography
-- and BLAKE2b for hashing.
module Vest.Prelude.Crypto
  ( module Reexports
  , hash256
  , SignedText'
  , seedKeyPair
  , sign'
  , verify'
  ) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import Crypto.Sign.Ed25519 as Reexports hiding (sign, sign', verify, verify')
import Vest.Prelude.Core

hash256 :: ByteString -> ByteString
-- ^ Output is 256 bits.
hash256 = BLAKE2b.hash 32 mempty

instance Hashable PublicKey

-- TODO: custom instance that doesn't include the unpublickey: thing
deriving instance Read PublicKey

instance ToJSON PublicKey

instance FromJSON PublicKey

instance ToJSONKey PublicKey

instance FromJSONKey PublicKey

deriving instance Read Signature

-- | We provide a signed type only for Texts, to avoid promoting extra serialization roundtrips
-- for verifying arbitrary types.
type SignedText' t = (Signature, Text' t)

seedKeyPair :: ByteString -> (PublicKey, SecretKey)
seedKeyPair = fromMaybe (panic "impossible") . createKeypairFromSeed_ . hash256

sign' :: SecretKey -> Text' t -> SignedText' t
sign' secret text' = (dsign secret (encodeUtf8 $ untag text'), text')

verify' :: PublicKey -> SignedText' t -> Maybe (Text' t)
verify' pubKey (sig, text') =
  if dverify pubKey (encodeUtf8 $ untag text') sig
    then Just text'
    else Nothing
