module Bridge.Auth.Open
  ( module Bridge.Auth.Open
  ) where

import Bridge.Prelude
import VestPrelude

verifyOpen :: Headers -> Text -> (Claims -> IO ()) -> IO () -> IO ()
verifyOpen _ _ action _ = action Claims
