module Bridge.Rpc.Auth.Token
  ( module Bridge.Rpc.Auth.Token
  ) where

import Bridge.Rpc.Prelude
import Vest.Prelude

data T

instance Auth T where
  data Claims T = Claims{}
  verify :: T -> Headers -> Text' "Request" -> Maybe (Claims T)
  verify _ _ _ = Just Claims
