module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import Data.Time.Clock as Reexports
import qualified Data.Vector as Vector
import Protolude as VestPrelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import System.Timeout as Reexports (timeout)
import Text.Read as Reexports (read, readMaybe)

-- If you want the Id or Route string you should destructure instead of using show:
-- show (Id "x") == "Id \"x\""
-- let Id text = (Id "x") -> text == "x"
newtype Id =
  Id Text
  deriving (Eq, Ord, Show, Read, Typeable, Hashable)

newtype Route =
  Route Text
  deriving (Eq, Ord, Show, Read, Typeable, Hashable)

-- timeouts are in micros
newtype Timeout =
  Timeout Int
  deriving (Eq, Ord, Show, Read, Typeable, Hashable)

instance Exception Timeout

repeatableTimeoutStream :: DiffTime -> IO (Maybe a -> IO (), Streamly.Serial a)
repeatableTimeoutStream _timeout = do
  let timeoutMicros =
        diffTimeToPicoseconds _timeout & fromIntegral & (* 1000000)
  resultsVar <- TVar.newTVarIO Vector.empty
  let push a = STM.atomically $ TVar.modifyTVar resultsVar (`Vector.snoc` a)
  let stream =
        Streamly.unfoldrM
          (\idx ->
             timeout
               timeoutMicros
               (STM.atomically $ do
                  results <- TVar.readTVar resultsVar
                  unless (idx < Vector.length results) STM.retry
                  case Vector.unsafeIndex results idx of
                    Nothing -> return Nothing
                    Just x -> return $ Just (x, idx + 1)) >>= \case
               Just result -> return result
               Nothing -> throwIO $ Timeout timeoutMicros)
          0
  return (push, stream)

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
