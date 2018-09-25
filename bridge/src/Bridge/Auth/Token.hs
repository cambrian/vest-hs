module Bridge.Auth.Token
  ( module Bridge.Auth.Token
  ) where

import Bridge.Prelude
import VestPrelude

-- TODO: Actually implement this!
verifyToken :: Headers -> Text -> (Claims -> IO ()) -> IO () -> IO ()
verifyToken _ _ action _ = action Claims
