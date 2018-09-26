module Bridge.Auth.Token
  ( module Bridge.Auth.Token
  ) where

import Bridge.Prelude
import VestPrelude

-- TODO:
data TokenAuth

data TokenClaims =
  TokenClaims

type instance Claims TokenAuth = TokenClaims

instance Auth TokenAuth where
  verify :: Headers -> Id "RequestText" -> IO (Maybe TokenClaims)
  verify _ _ = return $ Just TokenClaims
