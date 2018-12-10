-- | Crypto primitives.
-- Uses ed25519 for public key cryptography and BLAKE2b for hashing.
module Vest.Prelude.Crypto
  ( module Reexports
  , hash256
  , SignedText'
  , Seed
  , seedKeyPair
  , sign'
  , verify'
  ) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import Crypto.Sign.Ed25519 as Reexports hiding (sign, sign', verify, verify')
import Vest.Prelude.Core
import Vest.Prelude.Loadable

hash256 :: ByteString -> ByteString
-- ^ Output is 256 bits.
hash256 = BLAKE2b.hash 32 mempty

instance Hashable PublicKey

-- We don't use newtype deriving here because the Show instance doesn't, and we need show/read
-- to be round-trippable
deriving instance Read PublicKey

-- TODO: doctests for serialization roundtrips
deriving newtype instance ToJSON PublicKey

deriving newtype instance FromJSON PublicKey

deriving newtype instance ToJSONKey PublicKey

deriving newtype instance FromJSONKey PublicKey

deriving instance Read Signature

-- Loadable seed for a service's private/public key
newtype Seed = Seed
  { raw :: ByteString
  } deriving newtype (Eq, Read, Show, IsString, FromJSON, ToJSON)

instance SimpleLoadable Seed where
  file = [relfile|seed.yaml|]

instance Loadable Seed

seedKeyPair :: Seed -> (PublicKey, SecretKey)
seedKeyPair =
  fromMaybe (panic "impossible") . createKeypairFromSeed_ . hash256 . raw

-- | We provide a signed type only for Texts, to avoid promoting extra serialization roundtrips for
-- verifying arbitrary types.
type SignedText' t = (Signature, Text' t)

sign' :: SecretKey -> Text' t -> SignedText' t
sign' secret text' = (dsign secret $ encodeUtf8 $ untag text', text')

verify' :: PublicKey -> SignedText' t -> Maybe (Text' t)
verify' pubKey (sig, text') =
  if dverify pubKey (encodeUtf8 $ untag text') sig
    then Just text'
    else Nothing
