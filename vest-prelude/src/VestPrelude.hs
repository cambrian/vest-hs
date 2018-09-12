module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import qualified Control.Concurrent.Killable as Killable
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import Data.Aeson as Reexports
  ( FromJSON
  , ToJSON
  , (.=)
  , decode
  , encode
  , object
  , parseJSON
  , toJSON
  )
import Data.Hashable (Hashable)
import Data.Time.Clock as Reexports
import qualified Data.Vector as Vector
import Protolude as VestPrelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import System.Timeout as Reexports (timeout)
import qualified System.Timer.Updatable as UpdatableTimer
import Text.Read as Reexports (read, readMaybe)

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

-- Timeouts are in micros.
newtype Timeout =
  Timeout Int64
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Enum, Num)

instance Exception Timeout

instance Hashable Timeout

newtype URI =
  URI Text
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Hashable URI

diffTimeToMicros :: (Integral num) => DiffTime -> num
diffTimeToMicros x = fromIntegral (diffTimeToPicoseconds x) `quot` 1000000

timeoutThrowIO :: IO a -> DiffTime -> IO ()
timeoutThrowIO action _timeout = do
  let micros = diffTimeToMicros _timeout
  timer <- UpdatableTimer.replacer (throwIO (Timeout micros) :: IO ()) micros
  action >> Killable.kill timer

-- returns timeout renewer
timeoutThrowIO' :: IO a -> DiffTime -> IO (IO ())
timeoutThrowIO' action _timeout = do
  outerThreadId <- myThreadId
  let micros = diffTimeToMicros _timeout
  timer <-
    UpdatableTimer.replacer (throwTo outerThreadId (Timeout micros)) micros
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
