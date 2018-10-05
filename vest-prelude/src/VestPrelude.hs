module VestPrelude
  ( module VestPrelude
  , module Reexports
  ) where

import Control.Concurrent.Async as Reexports
import Control.Concurrent.MVar as Reexports
import Control.Concurrent.STM.Delay as Reexports
import Control.Concurrent.STM.TVar as Reexports
import qualified Control.Exception as Evil (Exception, throwTo)
import Control.Exception.Safe as Reexports
import qualified Control.Monad.STM as Reexports
import Crypto.Saltine as Reexports
import Data.Aeson as Reexports (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as ByteString.Lazy.UTF8
import Data.Hashable as Reexports (Hashable)
import Data.Pool
import Data.Pool as Reexports (Pool, tryWithResource, withResource)
import Data.Proxy as Reexports
import Data.Tagged as Reexports hiding (witness)
import Data.Text as Reexports (pack, unpack)
import Data.Time.Clock as Reexports (NominalDiffTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime, utcToSystemTime)
import Data.Type.Bool as Reexports
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Foreign.StablePtr as StablePtr
import qualified GHC.Base
import GHC.TypeLits as Reexports (AppendSymbol)
import Protolude as Reexports hiding
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
  , ignore
  , mask
  , mask_
  , moduleName
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
import System.Console.CmdArgs as Reexports
import qualified Text.Read
import Time.Rational (KnownDivRat)
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

class ManySymbolVal (xs :: [Symbol]) where
  manySymbolVal :: proxy xs -> [GHC.Base.String]

instance ManySymbolVal '[] where
  manySymbolVal _ = []

instance (KnownSymbol a, ManySymbolVal as) => ManySymbolVal (a ': as) where
  manySymbolVal _ =
    symbolVal (Proxy :: Proxy a) : manySymbolVal (Proxy :: Proxy as)

type Int' t = Tagged t Int

type Int64' t = Tagged t Int64

type Text' t = Tagged t Text

type IO' t a = Tagged t (IO a)

instance Hashable a => Hashable (Tagged s a)

data TxStatus
  = Pending
  | Confirmed
  | Rejected
  deriving (Eq, Ord, Read, Show, Enum, Generic, ToJSON, FromJSON, Hashable)

data BugException =
  BugException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

newtype TimeoutException =
  TimeoutException (Time Second)
  deriving ( Eq
           , Ord
           , Show
           , Read
           , Generic
           , Evil.Exception
           , Hashable
           , FromJSON
           , ToJSON
           )

-- DO NOT USE unless you really really know what you're doing.
evilThrowTo :: (Evil.Exception e) => ThreadId -> e -> IO ()
evilThrowTo = Evil.throwTo

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

blockForever :: IO Void
blockForever = do
  _ <- myThreadId >>= StablePtr.newStablePtr -- Stop the runtime from complaining that this thread
                                             -- is blocked forever by creating a stable reference
                                             -- to this thread that could conceivably be thrown to.
  atomically retry -- Block forever.

fromJustUnsafe :: (Exception e) => e -> Maybe a -> IO a
fromJustUnsafe e Nothing = throw e
fromJustUnsafe _ (Just a) = return a

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

newUUID :: IO (Text' t)
newUUID = UUID.nextRandom >>- (Tagged . UUID.toText)

data StreamPushAfterCloseException =
  StreamPushAfterCloseException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

-- Returns (push, close, stream).
-- Push throws if called after close is bound.
-- Close does nothing on repeated binds.
-- Remembers the most recent item pushed. A new consumption from this stream will see the most
-- recently pushed item, even if it is already closed.
pushStream :: IO (a -> IO (), IO' "CloseStream" (), Streamly.Serial a)
pushStream = do
  t <- newTVarIO (Nothing, False, 0 :: Word64) -- stores (Maybe value, closed, counter)
  let push a =
        atomically $ do
          (_, closed, counter) <- readTVar t
          when closed (throwSTM StreamPushAfterCloseException)
          writeTVar t (Just a, False, counter + 1)
      close =
        atomically $ modifyTVar' t (\(x, _, counter) -> (x, True, counter))
      stream =
        Streamly.unfoldrM
          (\highestSeen ->
             atomically $ do
               (value, closed, counter) <- readTVar t
               unless (closed || counter > highestSeen) retry
               if counter > highestSeen
                 then case value of
                        Just x -> return $ Just (x, counter)
                        Nothing -> throwSTM BugException
                 else return Nothing)
          0
  return (push, Tagged close, stream)

proxyText' :: (KnownSymbol a) => Proxy a -> Text' t
proxyText' = stringToText' . symbolVal

stringToText' :: GHC.Base.String -> Text' t
stringToText' = Tagged . pack

timeoutRenewable ::
     Time Second
  -> IO a
  -> IO (IO' "RenewTimeout" (), IO (Either TimeoutException a))
timeoutRenewable timeout_ action = do
  let micros = toNum @Microsecond timeout_
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
          Nothing -> Left $ TimeoutException timeout_
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

data PoolConfig = PoolConfig
  { idleTime :: Time Second
  , numResources :: Word
  -- numResources is technically per-stripe, but we just use 1 stripe.
  }

class Resource a where
  make :: ResourceConfig a -> IO a
  cleanup :: a -> IO ()
  -- ^ Minimal required definition.
  with :: ResourceConfig a -> (a -> IO b) -> IO b
  with config = bracket (make config) cleanup
  -- ^ TODO: Retry on exception.
  withPool :: PoolConfig -> ResourceConfig a -> (Pool a -> IO b) -> IO b
  -- ^ Use: withPool poolcfg cfg (\pool -> withResource pool (\resource -> do ...))
  withPool PoolConfig {idleTime, numResources} config =
    bracket
      (createPool (make config) cleanup 1 idleTime_ numResources_)
      destroyAllResources
    where
      idleTime_ = nominalDiffTimeFromTime idleTime
      numResources_ = fromIntegral numResources

type family ResourceConfig a = cfg | cfg -> a

type family ServiceArgs a = cfg | cfg -> a

type PublicKey a = Tagged a (Text' "PublicKey")

-- TODO: can we put shared argument logic here, like reading secret key files?
class (Data (ServiceArgs a), Typeable a) =>
      Service a
  where
  defaultArgs :: ServiceArgs a
  start_ :: ServiceArgs a -> (a -> IO Void) -> IO Void
  -- ^ This isn't the cleanest but I can't think of anything better for the time being.
  serviceName :: Text' "ServiceName"
  serviceName = moduleName @a
  start :: (a -> IO Void) -> IO Void
  start f = do
    args <- cmdArgs $ defaultArgs @a
    start_ args f

type family Symbols symbols where
  Symbols (Proxy c) = '[ c]
  Symbols (a
           :<|> b) = Symbols a :++ Symbols b

type family NubSymbols symbols where
  NubSymbols (Proxy c) = '[ c]
  NubSymbols (a
              :<|> b) = Nub (NubSymbols a :++ NubSymbols b)

type HasUniqueSymbols symbols = Symbols symbols ~ NubSymbols symbols

deriving instance Generic Timestamp

instance ToJSON Timestamp

instance FromJSON Timestamp

-- | Serialization
data SerializationFormat
  = Haskell
  | JSON
  deriving (Eq, Ord, Show, Read, Data, Generic, Hashable, ToJSON, FromJSON)

newtype DeserializeException (f :: SerializationFormat) =
  DeserializeException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

class Serializable (f :: SerializationFormat) a where
  serialize :: a -> Text
  serialize' :: a -> Text' t
  serialize' = Tagged . serialize @f

instance Show a => Serializable 'Haskell a where
  serialize = show

instance ToJSON a => Serializable 'JSON a where
  serialize = pack . ByteString.Lazy.UTF8.toString . Aeson.encode

class (Typeable f) =>
      Deserializable (f :: SerializationFormat) a
  where
  deserialize :: Text -> Maybe a
  deserialize' :: Text' t -> Maybe a
  deserialize' = deserialize @f . untag
  deserializeUnsafe :: Text -> IO a
  deserializeUnsafe text =
    fromJustUnsafe (DeserializeException @f text) $ deserialize @f text
  deserializeUnsafe' :: Text' t -> IO a
  deserializeUnsafe' = deserializeUnsafe @f . untag

instance Read a => Deserializable 'Haskell a where
  deserialize = Text.Read.readMaybe . unpack

instance FromJSON a => Deserializable 'JSON a where
  deserialize = Aeson.decode . ByteString.Lazy.UTF8.fromString . unpack

runtimeSerializationsOf' ::
     (Show a, Read b, ToJSON a, FromJSON b)
  => SerializationFormat
  -> (a -> Text' t, Text' v -> IO b)
-- This function is awkwardly named on purpose. Typically you know which serialization format
-- to use at compile time, but sometimes a runtime match is necessary.
runtimeSerializationsOf' Haskell =
  (serialize' @'Haskell, deserializeUnsafe' @'Haskell)
runtimeSerializationsOf' JSON = (serialize' @'JSON, deserializeUnsafe' @'JSON)

read :: (Read a) => Text -> Maybe a
read = deserialize @'Haskell

-- show is defined in protolude
-- Not providing encode/decode because you should prefer serialize/deserialize @'JSON
moduleName ::
     forall a t. Typeable a
  => Text' t
moduleName = Tagged . pack . dropLastToken_ . show $ typeRep (Proxy :: Proxy a)

dropLastToken_ :: GHC.Base.String -> GHC.Base.String
dropLastToken_ = reverse . tailSafe . dropWhile (/= '.') . reverse
