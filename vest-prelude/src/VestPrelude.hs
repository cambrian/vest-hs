module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import qualified Control.Concurrent.Killable as Killable
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Exception.Safe
import Control.Exception.Safe as Reexports
import qualified Control.Monad.STM as STM
import Data.Aeson as Reexports (FromJSON, ToJSON, decode, encode)
import Data.Hashable as Reexports (Hashable)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Protolude as VestPrelude hiding
  ( Exception
  , bracket
  , bracketOnError
  , bracketOnError_
  , bracket_
  , catch
  , catchAny
  , catchAsync
  , catchJust
  , catches
  , finally
  , handle
  , handleAny
  , handleAsync
  , handleJust
  , impureThrow
  , impureThrow
  , mask
  , mask_
  , onException
  , readMaybe
  , threadDelay
  , throw
  , throwIO
  , throwM
  , throwTo
  , throwTo
  , try
  , tryAny
  , tryAsync
  , tryIO
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  , withException
  )
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified System.Timer.Updatable as UpdatableTimer
import qualified Text.Read
import Time.Timestamp as Reexports
import Time.Units as Reexports

-- If you want the Id or Route string you should destructure instead of using show:
-- show (Id "x") == "Id \"x\""
-- let Id text = (Id "x") -> text == "x"
newtype Id =
  Id Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Id

instance FromJSON Id

instance ToJSON Id

newtype Route =
  Route Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Route

instance FromJSON Route

instance ToJSON Route

newtype Port =
  Port Int
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Port

newtype Timeout =
  Timeout (Time Second)
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception Timeout where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Hashable Timeout

newtype ReadException =
  ReadException Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception ReadException

newtype URI =
  URI Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable URI

timeoutThrowIO :: IO a -> Time Second -> IO ()
timeoutThrowIO action _timeout = do
  let micros = toNum @Microsecond _timeout
  timer <- UpdatableTimer.replacer (throwIO (Timeout _timeout) :: IO ()) micros
  action >> Killable.kill timer

-- Returns timeout renewer.
timeoutThrowIO' :: IO a -> Time Second -> IO (IO ())
timeoutThrowIO' action _timeout = do
  outerThreadId <- myThreadId
  let micros = toNum @Microsecond _timeout
  timer <-
    UpdatableTimer.replacer (throwTo outerThreadId (Timeout _timeout)) micros
  forkIO $ action >> Killable.kill timer
  return (UpdatableTimer.renewIO timer micros)

repeatableStream :: IO (Maybe a -> IO (), Streamly.Serial a)
repeatableStream = do
  resultsVar <- TVar.newTVarIO Vector.empty
  let push a = STM.atomically $ TVar.modifyTVar resultsVar (`Vector.snoc` a)
  let stream =
        Streamly.unfoldrM
          (\idx ->
             STM.atomically $ do
               results <- TVar.readTVar resultsVar
               unless (idx < Vector.length results) STM.retry
               case Vector.unsafeIndex results idx of
                 Nothing -> return Nothing
                 Just x -> return $ Just (x, idx + 1))
          0
  return (push, stream)

-- Creates a TVar that is updated with the latest value from as.
-- The TVar is Nothing until the first value is received.
makeStreamVar :: Streamly.Serial a -> IO (TVar.TVar (Maybe a))
makeStreamVar as = do
  var <- TVar.newTVarIO Nothing
  forkIO $ Streamly.mapM_ (STM.atomically . TVar.writeTVar var . Just) as
  return var

-- Creates a TVar that is updated with the latest value from as.
-- The TVar has an explicit initial value.
makeStreamVar' :: Streamly.Serial a -> a -> IO (TVar.TVar a)
makeStreamVar' as init = do
  var <- TVar.newTVarIO init
  forkIO $ Streamly.mapM_ (STM.atomically . TVar.writeTVar var) as
  return var

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

readUnsafe :: (Read a) => Text -> IO a
readUnsafe text =
  case readMaybe text of
    Nothing -> throwIO $ ReadException text
    Just x -> return x
