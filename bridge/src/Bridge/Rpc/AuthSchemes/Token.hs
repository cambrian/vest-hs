module Bridge.Rpc.AuthSchemes.Token
  ( module Bridge.Rpc.AuthSchemes.Token
  ) where

import Bridge.Rpc.Prelude
import VestPrelude

-- TODO:
data TokenAuth

data TokenClaims =
  TokenClaims

type instance Claims TokenAuth = TokenClaims

instance AuthScheme TokenAuth where
  verify :: Headers -> Text' "Request" -> IO (Maybe TokenClaims)
  verify _ _ = return $ Just TokenClaims
