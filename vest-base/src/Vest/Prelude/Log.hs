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
import Data.IORef
import System.IO.Unsafe
import Vest.Prelude.Core
import Vest.Prelude.Time

-- DO NOT DO THIS ON YOUR OWN!
logLockRef :: IORef Lock
{-# NOINLINE logLockRef #-}
logLockRef = unsafePerformIO (Lock.new >>= newIORef)

withLogLock :: IO a -> IO a
withLogLock action = do
  logLock <- readIORef logLockRef
  Lock.with logLock action

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
  withLogLock $ do
    time <- now
    putErrText $ show (time, level, context, a)

log_ :: LogLevel -> Text -> IO ()
log_ level context = log level context ()
