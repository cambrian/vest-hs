module Vest.Prelude.Time
  ( module Vest.Prelude.Time
  ) where

import qualified Control.Concurrent
import qualified Control.Exception as Evil (Exception)
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Data.Fixed
import Data.Time.Calendar (Day(..))
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime(..)
  , addUTCTime
  , diffUTCTime
  , getCurrentTime
  )
import qualified System.Timeout
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read
import Vest.Prelude.Core

instance TypeScript UTCTime where
  getTypeScriptType _ = "string"

-- | TODO: doctest this
instance Read NominalDiffTime where
  readPrec = do
    n <- readPrec @Pico
    ReadPrec.lift $ do
      void $ ReadP.char 's'
      ReadP.eof
    return $ fromRational $ toRational n

type Time = UTCTime

type Duration = NominalDiffTime

newtype TimeoutException =
  TimeoutException Duration
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Evil.Exception, ToJSON, FromJSON)

instance VectorSpace Duration where
  type Scalar Duration = Rational
  a *^ t = fromRational a * t

instance VectorDivisible Duration where
  a ^/^ b = toRational a / toRational b

instance AffineSpace Time where
  type Diff Time = Duration
  (.-.) = diffUTCTime
  (.+^) = flip addUTCTime

sec :: Rational -> Duration
sec = fromRational

ms :: Rational -> Duration
ms = fromRational . (/ 1e3)

day :: Rational -> Duration
day = fromRational . (86400 *)

durationMicros :: Duration -> Int
durationMicros t = round $ 1e6 * toRational t

durationMillis :: Duration -> Int
durationMillis t = round $ 1e3 * toRational t

timeout :: Duration -> IO a -> IO (Either TimeoutException a)
  -- ^ returns Left TimeoutException if f does not complete within the given time
timeout t f =
  System.Timeout.timeout (durationMicros t) f >>- \case
    Just a -> Right a
    Nothing -> Left $ TimeoutException t

threadDelay :: Duration -> IO ()
threadDelay = Control.Concurrent.threadDelay . durationMicros

timeoutRenewable ::
     Duration -> IO a -> IO (Duration -> IO (), IO (Either TimeoutException a))
timeoutRenewable t f = do
  delay <- newDelay $ durationMicros t
  resultMVar <- newEmptyMVar
  void . async $ do
    atomically $ waitDelay delay
    putMVar resultMVar Nothing
  void . async $ do
    result <- f
    cancelDelay delay
    putMVar resultMVar (Just result)
  let renew newTimeout = updateDelay delay $ durationMicros newTimeout
  let result =
        readMVar resultMVar >>- \case
          Nothing -> Left $ TimeoutException t
          Just a -> Right a
  return (renew, result)

data ZeroIntervalException =
  ZeroIntervalException
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, Exception)

timeMin :: Time
timeMin = UTCTime (ModifiedJulianDay 0) 0

intervalRenewable ::
     Duration -> IO () -> IO (IO' "RenewInterval" (), IO' "CancelInterval" ())
-- ^ Begins by waiting; does not execute immediately. Interval cannot be zero; the function will
-- throw at runtime.
intervalRenewable interval action = do
  when (interval == 0) $ throw ZeroIntervalException
  nextBeat <- newTVarIO timeMin
  let renew = do
        time <- now
        atomically $ writeTVar nextBeat $ time .+^ interval
  renew
  intervalThread <-
    async . forever $ do
      time <- now
      nextBeatTime <- readTVarIO nextBeat
      if time > nextBeatTime
        then do
          void $ async action
          atomically $ writeTVar nextBeat $ nextBeatTime .+^ interval
          threadDelay interval
        else threadDelay $ nextBeatTime .-. time
  return (Tagged renew, Tagged $ uninterruptibleCancel intervalThread)

now :: IO Time
now = getCurrentTime

natSeconds ::
     forall seconds. KnownNat seconds
  => Duration
natSeconds = fromIntegral $ natVal (Proxy :: Proxy seconds)
