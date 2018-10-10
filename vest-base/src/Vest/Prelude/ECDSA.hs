module Vest.Prelude.ECDSA
  ( module Reexports
  ) where

import Crypto.PubKey.ECC.ECDSA as Reexports
import qualified Crypto.PubKey.ECC.Types as Types
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

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
