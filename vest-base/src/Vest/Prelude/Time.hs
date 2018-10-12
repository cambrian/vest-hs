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
import Time.Units as Vest.Prelude.Time

deriving instance Generic Timestamp

instance ToJSON Timestamp

instance FromJSON Timestamp

newtype TimeoutException =
  TimeoutException (Time Second)
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Evil.Exception, Hashable, FromJSON, ToJSON)

timeoutRenewable ::
     Time Second
  -> IO a
  -> IO (IO' "RenewTimeout" (), IO (Either TimeoutException a))
timeoutRenewable timeout action = do
  let micros = toNum @Microsecond timeout
  delay <- newDelay micros
  resultMVar <- newEmptyMVar
  void . async $ do
    atomically $ waitDelay delay
    putMVar resultMVar Nothing
  void . async $ do
    result <- action
    cancelDelay delay
    putMVar resultMVar (Just result)
  let result =
        readMVar resultMVar >>- \case
          Nothing -> Left $ TimeoutException timeout
          Just a -> Right a
  return (Tagged $ updateDelay delay micros, result)

-- UTCTime <--> Timestamp
-- TODO: keep nanos
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
