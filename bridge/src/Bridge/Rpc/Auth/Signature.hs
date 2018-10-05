module Bridge.Rpc.Auth.Signature
  ( module Bridge.Rpc.Auth.Signature
  ) where

import Bridge.Rpc.Prelude
import VestPrelude

data T

data Claims =
  Claims

type instance AuthClaims T = Claims

instance Auth T where
  verify :: Headers -> Text' "Request" -> IO (Maybe Claims)
  verify _ _ = return $ Just Claims