module Vest.Logger
  ( module Vest.Logger
  ) where

import Vest.Prelude

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON)

newtype Logger = Logger
  { log :: LogLevel -> Text -> IO ()
  }

class HasLogger t where
  logger :: t -> Logger
