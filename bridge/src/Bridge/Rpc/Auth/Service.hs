module Bridge.Rpc.Auth.Service
  ( module Bridge.Rpc.Auth.Service
  ) where

import Bridge.Rpc.Prelude
import VestPrelude

data T service

data Claims service =
  Claims

type instance AuthClaims (T service) = Claims service

instance Auth (T a) where
  verify :: Headers -> Text' "Request" -> IO (Maybe (Claims service))
  verify _ _ = return $ Just Claims
