module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Async as Reexports
import qualified Control.Concurrent.Killable as Killable
import Control.Concurrent.MVar as REexports
import Control.Concurrent.STM.TVar as Reexports
import Control.Exception.Safe as Reexports
import qualified Control.Monad.STM as Reexports
import Data.Aeson as Reexports (FromJSON, ToJSON, decode, encode)
import Data.Hashable as Reexports (Hashable)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Foreign.StablePtr as StablePtr
import Protolude as VestPrelude hiding
  ( Exception
  , bracket
  , bracketOnError
  , bracket_
  , catch
  , catchJust
  , catches
  , finally
  , forkIO
  , handle
  , handleJust
  , mask
  , mask_
  , onException
  , readMaybe
  , threadDelay
  , throwIO
  , throwTo
  , try
  , tryIO
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  )
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import qualified System.Timer.Updatable as UpdatableTimer
import qualified Text.Read
import Time.Timestamp as Reexports
import Time.Units as Reexports

data Currency
  = USD
  | BTC
  | ETH
  | XTZ
  deriving (Eq, Ord, Show, Read, Enum, Generic, Typeable)

instance Hashable Currency

instance ToJSON Currency

instance FromJSON Currency

-- If you want the Id or Route string you should destructure instead of using show:
-- show (Id "x") == "Id \"x\""
-- let Id text = (Id "x") -> text == "x"
newtype Id =
  Id Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Id

instance FromJSON Id

instance ToJSON Id

newtype Port =
  Port Int
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Port

newtype Route =
  Route Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable Route

instance FromJSON Route

instance ToJSON Route

newtype URI =
  URI Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable URI

newtype DecodeException =
  DecodeException Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception DecodeException

data NothingException =
  NothingException
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception NothingException

newtype ReadException =
  ReadException Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception ReadException

newtype Timeout =
  Timeout (Time Second)
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception Timeout where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Hashable Timeout

newtype UnsupportedCurrencyException =
  UnsupportedCurrencyException Currency
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Exception UnsupportedCurrencyException

newtype URI =
  URI Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

blockForever :: IO ()
blockForever = do
  _ <- myThreadId >>= StablePtr.newStablePtr -- Stop the runtime from complaining that this thread
                                             -- is blocked forever by creating a stable reference
                                             -- to this thread that could conceivably be thrown to.
  atomically retry -- Block forever.

(+++) :: Text -> Text -> Text
(+++) = Text.append

fromJustUnsafe :: Maybe a -> IO a
fromJustUnsafe Nothing = throwIO NothingException
fromJustUnsafe (Just a) = return a

-- Creates a TVar that is updated with the latest value from as.
-- The TVar is Nothing until the first value is received.
makeStreamVar :: Streamly.Serial a -> IO (TVar (Maybe a))
makeStreamVar as = do
  var <- newTVarIO Nothing
  async $ Streamly.mapM_ (atomically . writeTVar var . Just) as
  return var

-- Creates a TVar that is updated with the latest value from as.
-- The TVar has an explicit initial value.
makeStreamVar' :: Streamly.Serial a -> a -> IO (TVar a)
makeStreamVar' as init = do
  var <- newTVarIO init
  async $ Streamly.mapM_ (atomically . writeTVar var) as
  return var

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = Text.Read.readMaybe . Text.unpack

readUnsafe :: (Read a) => Text -> IO a
readUnsafe text =
  case readMaybe text of
    Nothing -> throwIO $ ReadException text
    Just x -> return x

-- Returns (push, close, stream).
-- Push does nothing after close is bound.
repeatableStream :: IO (a -> IO (), IO (), Streamly.Serial a)
repeatableStream = do
  resultsVar <- newTVarIO Vector.empty
  let push a = atomically $ modifyTVar resultsVar (`Vector.snoc` a)
      stream =
        Streamly.unfoldrM
          (\idx ->
             atomically $ do
               results <- readTVar resultsVar
               unless (idx < Vector.length results) retry
               return $ fmap (, idx + 1) (Vector.unsafeIndex results idx))
          0
  return (push . Just, push Nothing, stream)

timeoutThrow :: IO a -> Time Second -> IO ()
timeoutThrow action _timeout = do
  let micros = toNum @Microsecond _timeout
  timer <- UpdatableTimer.replacer (throw (Timeout _timeout) :: IO ()) micros
  action >> Killable.kill timer

-- Returns timeout renewer.
timeoutThrow' :: IO a -> Time Second -> IO (IO ())
timeoutThrow' action _timeout = do
  outerThreadId <- myThreadId
  let micros = toNum @Microsecond _timeout
  timer <-
    UpdatableTimer.replacer (throwTo outerThreadId (Timeout _timeout)) micros
  async $ action >> Killable.kill timer
  return (UpdatableTimer.renewIO timer micros)
