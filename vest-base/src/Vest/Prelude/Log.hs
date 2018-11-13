-- | All logs go to stderr, so services don't have to think about or maintain connections to any
-- fancy log stores. To use a fancy log store, read a service's stderr from another process.
module Vest.Prelude.Log
  ( module Vest.Prelude.Log
  ) where

import Vest.Prelude.Core
import Vest.Prelude.Time

data LogLevel
  = Debug
  | Warn
  | Error
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON)

type LogMessage a = (Time, LogLevel, a)

log :: Show a => LogLevel -> a -> IO ()
log level a = do
  time <- now
  putErrText $ show (time, level, a)
