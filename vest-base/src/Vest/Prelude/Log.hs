-- | All logs go to stderr, so services don't have to think about or maintain connections to any
-- fancy log stores. To use a fancy log store, read a service's stderr from another process.
module Vest.Prelude.Log
  ( LogLevel(..)
  , LogMessage
  , log
  , log_
  ) where

import Data.IORef
import System.IO.Unsafe
import Vest.Prelude.Core
import Vest.Prelude.Time

-- DO NOT DO THIS ON YOUR OWN!
logLockRef :: IORef (TMVar ())
{-# NOINLINE logLockRef #-}
logLockRef = unsafePerformIO (newEmptyTMVarIO >>= newIORef)

withLogLock :: IO a -> IO a
withLogLock action = do
  logLock <- readIORef logLockRef
  atomically $ putTMVar logLock ()
  result <- action
  void . atomically $ takeTMVar logLock
  return result

getTestMode :: IO Bool
getTestMode = isJust <$> lookupEnv "___TEST_MODE_DO_NOT_SET"

data LogLevel
  = Debug
  | Warn
  | Error
  deriving (Eq, Ord, Enum, Show, Read, Generic, ToJSON, FromJSON)

type LogMessage a = (Time, LogLevel, a)

log :: Show a => LogLevel -> Text -> a -> IO ()
log level context a =
  unlessM getTestMode $
  withLogLock $ do
    time <- now
    putErrText $ show (time, level, context, a)

log_ :: LogLevel -> Text -> IO ()
log_ level context = log level context ()
