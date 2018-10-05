module Bridge.Rpc.Auth.Signature
  ( module Bridge.Rpc.Auth.Signature
  ) where

import Bridge.Rpc.Prelude
import VestPrelude

data T

instance Auth T where
  data Claims T = Claims{}
  verify :: Headers -> Text' "Request" -> IO (Maybe (Claims T))
  verify _ _ = return $ Just Claims
