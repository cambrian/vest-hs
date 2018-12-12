-- | All logs go to stderr, so services don't have to think about or maintain connections to any
-- fancy log stores. To use a fancy log store, read a service's stderr from another process.
module Vest.Prelude.Log
  ( LogLevel(..)
  , LogMessage
  , log
  , log_
  ) where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import System.IO.Unsafe (unsafePerformIO)
import Vest.Prelude.Core
import Vest.Prelude.Time

logLock :: Lock
{-# NOINLINE logLock #-}
logLock = unsafePerformIO Lock.new

shouldSwallowLogs :: IO Bool
shouldSwallowLogs = isJust <$> lookupEnv "VEST_SWALLOW_LOGS"

data LogLevel
  = Debug
  | Warn
  | Error
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON)

type LogMessage a = (Time, LogLevel, a)

log :: Show a => LogLevel -> Text -> a -> IO ()
log level context a =
  unlessM shouldSwallowLogs $
  Lock.with logLock $ do
    time <- now
    putErrText $ show (time, level, context, a)

log_ :: LogLevel -> Text -> IO ()
log_ level context = log level context ()
