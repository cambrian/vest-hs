module Bridge.Rpc.Auth.Internal
  ( module Bridge.Rpc.Auth.Internal
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
