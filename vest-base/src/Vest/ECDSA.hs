module Vest.ECDSA
  ( module Reexports
  , SignedText'
  , sign'
  , verify'
  ) where

import Crypto.Hash.Algorithms (Blake2b_256(..))
import Crypto.PubKey.ECC.ECDSA as Reexports
  ( KeyPair
  , PrivateKey
  , PublicKey
  , Signature
  , toPrivateKey
  , toPublicKey
  )
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as Types
import Data.Aeson (FromJSONKey, ToJSONKey)
import Vest.Prelude

deriving instance Generic Types.CurveBinary

deriving instance ToJSON Types.CurveBinary

deriving instance FromJSON Types.CurveBinary

deriving instance Generic Types.CurveCommon

deriving instance ToJSON Types.CurveCommon

deriving instance FromJSON Types.CurveCommon

deriving instance Generic Types.CurvePrime

deriving instance ToJSON Types.CurvePrime

deriving instance FromJSON Types.CurvePrime

deriving instance Generic Types.Curve

deriving instance ToJSON Types.Curve

deriving instance FromJSON Types.Curve

deriving instance Generic Types.Point

deriving instance ToJSON Types.Point

deriving instance FromJSON Types.Point

deriving instance Generic PublicKey

deriving instance ToJSON PublicKey

deriving instance FromJSON PublicKey

deriving instance Generic PrivateKey

deriving instance ToJSON PrivateKey

deriving instance FromJSON PrivateKey

deriving instance Generic KeyPair

deriving instance ToJSON KeyPair

deriving instance FromJSON KeyPair

deriving instance Hashable Types.CurveCommon

deriving instance Hashable Types.CurvePrime

deriving instance Hashable Types.CurveBinary

deriving instance Hashable Types.Point

deriving instance Hashable Types.Curve

deriving instance Hashable PublicKey

deriving instance ToJSONKey Types.CurveCommon

deriving instance ToJSONKey Types.CurvePrime

deriving instance ToJSONKey Types.CurveBinary

deriving instance ToJSONKey Types.Point

deriving instance ToJSONKey Types.Curve

deriving instance ToJSONKey PublicKey

deriving instance FromJSONKey Types.CurveCommon

deriving instance FromJSONKey Types.CurvePrime

deriving instance FromJSONKey Types.CurveBinary

deriving instance FromJSONKey Types.Point

deriving instance FromJSONKey Types.Curve

deriving instance FromJSONKey PublicKey

-- | We provide a signed type only for Texts, to avoid promoting extra serialization roundtrips
-- for verifying arbitrary types.
type SignedText' t = (Signature, Text' t)

sign' :: PrivateKey -> Text' t -> IO (SignedText' t)
-- ^ signing is an IO operation because it requires randomness.
sign' privKey text' = do
  sig <- ECDSA.sign privKey Blake2b_256 (encodeUtf8 $ untag text')
  return (sig, text')

verify' :: PublicKey -> SignedText' t -> Maybe (Text' t)
verify' pubKey (sig, text') =
  if ECDSA.verify Blake2b_256 pubKey sig (encodeUtf8 $ untag text')
    then Just text'
    else Nothing
