module Bridge.Rpc.Auth.Signature
  ( module Bridge.Rpc.Auth.Signature
  ) where

import Bridge.Rpc.Prelude
import VestPrelude

data T

instance Auth T where
  data AuthClaims T = Claims{}
  verify :: Headers -> Text' "Request" -> IO (Maybe (AuthClaims T))
  verify _ _ = return $ Just Claims
