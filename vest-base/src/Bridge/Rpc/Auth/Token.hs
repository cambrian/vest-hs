module Bridge.Rpc.Auth.Token
  ( module Bridge.Rpc.Auth.Token
  ) where

import Bridge.Rpc.Prelude
import Vest.Prelude

data T

instance Auth T where
  data Claims T = Claims{}
  verify :: Headers -> Text' "Request" -> IO (Maybe (Claims T))
  verify _ _ = return $ Just Claims
