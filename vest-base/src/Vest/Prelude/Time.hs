module Vest.Prelude.Time
  ( module Vest.Prelude.Time
  ) where

import Vest.Prelude.Core

import qualified Control.Exception as Evil (Exception)
import Data.Time.Clock as Vest.Prelude.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime, utcToSystemTime)
import Time.Rational (KnownDivRat)
import Time.Timestamp as Vest.Prelude.Time
import Time.Units as Vest.Prelude.Time hiding (timeout)
import qualified Time.Units

deriving instance Generic Timestamp

instance ToJSON Timestamp

instance FromJSON Timestamp

newtype TimeoutException unit =
  TimeoutException (Time unit)
  deriving (Eq, Ord, Generic)
  deriving anyclass (Hashable)

deriving instance
         KnownUnitName unit => Read (TimeoutException unit)

deriving instance
         KnownUnitName unit => Show (TimeoutException unit)

instance (Typeable unit, KnownUnitName unit) =>
         Evil.Exception (TimeoutException unit)

instance KnownUnitName unit => ToJSON (TimeoutException unit)

instance KnownUnitName unit => FromJSON (TimeoutException unit)

timeout ::
     KnownDivRat unit Microsecond
  => Time unit -- ^ time
  -> IO a -- ^ 'IO' action
  -> IO (Either (TimeoutException unit) a) -- ^ returns Nothing if no result is available within the given time
timeout t action =
  Time.Units.timeout t action >>- \case
    Just a -> Right a
    Nothing -> Left $ TimeoutException t

timeoutRenewable ::
     KnownDivRat unit Microsecond
  => Time unit
  -> IO a
  -> IO (Time unit -> IO (), IO (Either (TimeoutException unit) a))
timeoutRenewable t action = do
  let micros = toNum @Microsecond
  delay <- newDelay $ micros t
  resultMVar <- newEmptyMVar
  void . async $ do
    atomically $ waitDelay delay
    putMVar resultMVar Nothing
  void . async $ do
    result <- action
    cancelDelay delay
    putMVar resultMVar (Just result)
  let renew newTimeout = updateDelay delay $ micros newTimeout
  let result =
        readMVar resultMVar >>- \case
          Nothing -> Left $ TimeoutException t
          Just a -> Right a
  return (renew, result)

throwIfTimeout ::
     (Typeable unit, KnownUnitName unit)
  => Either (TimeoutException unit) a
  -> IO a
throwIfTimeout (Left exn) = throw exn
throwIfTimeout (Right a) = return a

intervalRenewable ::
     (KnownDivRat unit Microsecond, KnownDivRat unit Second)
  => Time unit
  -> IO ()
  -> IO (IO' "RenewInterval" (), IO' "CancelInterval" ())
-- ^ Begins by waiting; does not execute immediately.
intervalRenewable interval action = do
  nextBeat <- newTVarIO (Timestamp 0)
  let renew = do
        time <- now
        atomically $ writeTVar nextBeat (timeAdd interval time)
  renew
  intervalThread <-
    async . forever $ do
      time <- now
      nextBeatTime <- readTVarIO nextBeat
      if time > nextBeatTime
        then do
          void $ async action
          atomically $ writeTVar nextBeat (timeAdd interval nextBeatTime)
          threadDelay interval
        else threadDelay (snd $ timeDiff @Second time nextBeatTime)
  return (Tagged renew, Tagged $ uninterruptibleCancel intervalThread)

-- UTCTime <--> Timestamp
-- TODO: Keep nanos.
utcTimeFromTimestamp :: Timestamp -> UTCTime
utcTimeFromTimestamp (Timestamp seconds) =
  systemToUTCTime $
  MkSystemTime {systemSeconds = truncate seconds, systemNanoseconds = 0}

timestampFromUTCTime :: UTCTime -> Timestamp
timestampFromUTCTime utcTime =
  let MkSystemTime {systemSeconds} = utcToSystemTime utcTime
   in fromUnixTime systemSeconds

nominalDiffTimeFromTime ::
     (KnownDivRat unit Second) => Time unit -> NominalDiffTime
nominalDiffTimeFromTime = fromRational . toNum @Second

now :: IO Timestamp
now = getCurrentTime >>- timestampFromUTCTime

natSeconds ::
     forall seconds. KnownNat seconds
  => Time Second
natSeconds = sec $ fromIntegral $ natVal (Proxy :: Proxy seconds)
