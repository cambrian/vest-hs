module Vest.Logger
  ( module Vest.Logger
  ) where

import Vest.Prelude

data LogLevel
  = Debug
  | Warn
  | Error
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON)

newtype Logger = Logger
  { log_ :: LogLevel -> Text -> IO ()
  }

instance Semigroup Logger where
  (<>) a b = Logger $ \level msg -> log_ a level msg >> log_ b level msg

instance Monoid Logger where
  mempty = Logger $ const $ const $ return ()

stderrLogger :: LogLevel -> Logger
stderrLogger level = Logger $ \lvl msg -> unless (lvl < level) $ putErrText msg
