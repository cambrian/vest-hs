module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import Control.Concurrent.Async as Reexports
import qualified Control.Concurrent.Killable as Killable
import Control.Concurrent.MVar as Reexports
import Control.Concurrent.STM.TVar as Reexports
import Control.Exception.Safe as Reexports
import qualified Control.Monad.STM as Reexports
import Data.Aeson as Reexports (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import Data.Hashable as Reexports (Hashable)
import Data.Proxy as Reexports
import Data.Text as Reexports (pack, unpack)
import Data.Type.Bool as Reexports
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import qualified Foreign.StablePtr as StablePtr
import GHC.TypeLits
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

-- Checks if x in xs at type level.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- De-duplicates xs at type level.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = If (Elem x xs) (Nub xs) (x ': Nub xs)

-- Type-level array concatenation using :++.
type family (x :: [k]) :++ (y :: [k]) :: [k] where
  '[] :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

-- For all of these wrapper types, you can extract the base Text by deconstructing:
-- show (Id "x") == "Id \"x\""
-- let Id text = (Id "x") -> text == "x"
newtype Id =
  Id Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

newtype Host =
  Host Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

newtype Hash =
  Hash Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

newtype Port =
  Port Int
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

newtype Route =
  Route Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

newtype URI =
  URI Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data Format
  = Haskell
  | JSON
  deriving (Eq, Ord, Show, Read, Enum, Generic, Hashable, ToJSON, FromJSON)

newtype DecodeException =
  DecodeException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

data NothingException =
  NothingException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

newtype ReadException =
  ReadException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

newtype TimeoutException =
  TimeoutException (Time Second)
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

instance Exception TimeoutException where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

data a :<|> b =
  a :<|> b

infixr 8 :<|>

infixl 1 >>-

-- Infix map with arguments flipped. Like (>>=), but the chained function is pure.
(>>-) :: (Functor f) => f a -> (a -> b) -> f b
(>>-) ma f = map f ma

infixl 1 >|>

-- Infix forM.
(>|>) :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
(>|>) = forM

infixl 1 >|>|

-- Infix forM_.
(>|>|) :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
(>|>|) = forM_

blockForever :: IO ()
blockForever = do
  _ <- myThreadId >>= StablePtr.newStablePtr -- Stop the runtime from complaining that this thread
                                             -- is blocked forever by creating a stable reference
                                             -- to this thread that could conceivably be thrown to.
  atomically retry -- Block forever.

decodeMaybe :: (FromJSON a) => Text -> Maybe a
decodeMaybe = Aeson.decode . ByteString.Lazy.UTF8.fromString . unpack

decode :: (FromJSON a) => Text -> Maybe a
decode = decodeMaybe

decodeUnsafe :: (FromJSON a) => Text -> IO a
decodeUnsafe text =
  case decodeMaybe text of
    Nothing -> throw $ DecodeException text
    Just x -> return x

encode :: (ToJSON a) => a -> Text
encode = pack . ByteString.Lazy.UTF8.toString . Aeson.encode

fromJustUnsafe :: Maybe a -> IO a
fromJustUnsafe Nothing = throw NothingException
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
makeStreamVar' :: a -> Streamly.Serial a -> IO (TVar a)
makeStreamVar' init as = do
  var <- newTVarIO init
  async $ Streamly.mapM_ (atomically . writeTVar var) as
  return var

newUuid :: IO Id
newUuid = UUID.nextRandom >>- (Id . UUID.toText)

proxyText :: (KnownSymbol a) => Proxy a -> Text
proxyText = pack . symbolVal

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = Text.Read.readMaybe . unpack

read :: (Read a) => Text -> Maybe a
read = readMaybe

readUnsafe :: (Read a) => Text -> IO a
readUnsafe text =
  case readMaybe text of
    Nothing -> throw $ ReadException text
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
  timer <-
    UpdatableTimer.replacer (throw (TimeoutException _timeout) :: IO ()) micros
  action >> Killable.kill timer

-- Returns timeout renewer.
timeoutThrow' :: IO a -> Time Second -> IO (IO ())
timeoutThrow' action _timeout = do
  outerThreadId <- myThreadId
  let micros = toNum @Microsecond _timeout
  timer <-
    UpdatableTimer.replacer
      (throwTo outerThreadId (TimeoutException _timeout))
      micros
  async $ action >> Killable.kill timer
  return (UpdatableTimer.renewIO timer micros)

class Resource t where
  type ResourceConfig t :: *
  hold :: ResourceConfig t -> IO t
  release :: t -> IO ()
  -- ^ Minimal required definition
  with :: ResourceConfig t -> (t -> IO ()) -> IO ()
  with config = bracket (hold config) release
  -- TODO: catch exceptions and reconnect
  withForever :: ResourceConfig t -> (t -> IO ()) -> IO ()
  withForever config action =
    bracket (hold config) release (\t -> action t >> blockForever)
