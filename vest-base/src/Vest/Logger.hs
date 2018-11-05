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

class Logger a where
  log :: a -> LogLevel -> Text -> IO ()

class HasLogger a t where
  logger :: t -> a
