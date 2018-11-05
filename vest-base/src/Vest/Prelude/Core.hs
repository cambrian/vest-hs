-- Module for super base things. This file should be mostly reexports.
module Vest.Prelude.Core
  ( module Vest.Prelude.Core
  ) where

import Control.Concurrent.Async as Vest.Prelude.Core
import Control.Concurrent.STM.Delay as Vest.Prelude.Core
import Control.Concurrent.STM.TMVar as Vest.Prelude.Core
import Control.Concurrent.STM.TVar as Vest.Prelude.Core
import qualified Control.Exception as Evil (Exception, throwTo)
import Control.Exception.Safe as Vest.Prelude.Core
import Control.Monad.STM as Vest.Prelude.Core
import Control.Monad.Trans.Maybe as Vest.Prelude.Core
import Data.Aeson.TypeScript.TH
import Data.HashMap.Strict as Vest.Prelude.Core (HashMap)
import Data.HashSet as Vest.Prelude.Core (HashSet)
import qualified Data.HashTable.IO as HashTable
import Data.HashTable.IO as Vest.Prelude.Core (CuckooHashTable, LinearHashTable)
import Data.Hashable as Vest.Prelude.Core (Hashable(..))
import Data.Proxy as Vest.Prelude.Core
import Numeric.Natural as Vest.Prelude.Core
import qualified Protolude
import Protolude as Vest.Prelude.Core hiding
  ( bracket
  , bracketOnError
  , bracket_
  , catch
  , catchJust
  , catches
  , finally
  , handle
  , handleJust
  , log -- clashes with Logger.log
  , mask
  , mask_
  , moduleName
  , onException
  , show -- Redefine text-specialized show.
  , threadDelay
  , throwIO
  , throwTo
  , try
  , tryIO
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  )
import qualified TMap

-- import qualified Control.Monad.Fail
import Control.Monad.Fail as Vest.Prelude.Core
import Control.Monad.ST as Vest.Prelude.Core
import Control.Monad.State as Vest.Prelude.Core hiding (fail)
import Data.Aeson as Vest.Prelude.Core
  ( FromJSON(..)
  , FromJSONKey(..)
  , ToJSON(..)
  , ToJSONKey(..)
  )
import Data.Aeson (withText)
import Data.Aeson.Types (FromJSONKeyFunction(..), defaultOptions, toJSONKeyText)
import qualified Data.ByteString.Base64 as Base64
import Data.Data as Vest.Prelude.Core (Data(..))
import Data.STRef as Vest.Prelude.Core
import Data.String as Vest.Prelude.Core (IsString(..))
import Data.String.Conversions as Vest.Prelude.Core hiding
  ( LBS
  , LT
  , SBS
  , ST
  , cs
  ) -- hide confusing abbreviations
import Data.Tagged as Vest.Prelude.Core hiding (witness)
import Data.Text as Vest.Prelude.Core (pack, unpack)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Typeable (tyConModule, typeRepTyCon)
import qualified Foreign.StablePtr as StablePtr
import Network.HostName (getHostName)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid(CPid))
import System.Random as Vest.Prelude.Core

type Int' t = Tagged t Int

type Word16' t = Tagged t Word16

type Int64' t = Tagged t Int64

type Integer' t = Tagged t Integer

type Text' t = Tagged t Text

type ByteString' t = Tagged t ByteString

type IO' t a = Tagged t (IO a)

type STM' t a = Tagged t (STM a)

type Async' t a = Tagged t (Async a)

async' :: IO a -> IO (Async' t a)
async' x = Tagged <$> async x

instance Hashable a => Hashable (Tagged s a)

type HashTable k v = HashTable.BasicHashTable k v

type TMap = TMap.TMap

data BugException =
  BugException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

-- Used to avoid overlapping instances when complementary constraints cannot be expressed to GHC.
data Raw a

-- Aeson instances copied from base64-bytestring-type.
-- ByteStrings are serialized as Latin-1 (ASCII) Texts, after base64 encoding.
instance ToJSON ByteString where
  toJSON = toJSON . decodeLatin1 . Base64.encode
  toEncoding = toEncoding . decodeLatin1 . Base64.encode

instance FromJSON ByteString where
  parseJSON =
    withText "ByteString" $ either fail pure . Base64.decode . encodeUtf8

instance TypeScript ByteString where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy Text)
  getTypeScriptDeclarations _ = getTypeScriptDeclarations (Proxy :: Proxy Text)

instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText (decodeLatin1 . Base64.encode)

instance FromJSONKey ByteString where
  fromJSONKey =
    FromJSONKeyTextParser $ either fail pure . Base64.decode . encodeUtf8

-- Symbols will show up in TS as string literals.
instance (KnownSymbol s) => TypeScript (s :: Symbol) where
  getTypeScriptType s = "\"" ++ symbolVal s ++ "\""

$(deriveTypeScript defaultOptions ''Tagged)

evilThrowTo :: (Evil.Exception e) => ThreadId -> e -> IO ()
-- ^ DO NOT USE unless you really really know what you're doing.
evilThrowTo = Evil.throwTo

fromJustUnsafe :: (Exception e) => e -> Maybe a -> IO a
fromJustUnsafe e Nothing = throw e
fromJustUnsafe _ (Just a) = return a

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l Nothing = Left l
eitherFromMaybe _ (Just r) = Right r

fromRightOrThrowLeft :: (Exception e) => Either e a -> IO a
fromRightOrThrowLeft (Left e) = throw e
fromRightOrThrowLeft (Right a) = return a

justSTM :: STM (Maybe a) -> STM a
-- ^ Retries until Just is received
justSTM s =
  s >>= \case
    Nothing -> retry
    Just a -> return a

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t a = tryTakeTMVar t >> putTMVar t a

-- | Infix map, like (>>=) but pure.
-- Note: It's usually the case that f <$> a = a >>- f
(>>-) :: (Functor f) => f a -> (a -> b) -> f b
(>>-) = flip map

infixl 1 >>-

blockForever :: IO Void
blockForever = do
  _ <- myThreadId >>= StablePtr.newStablePtr -- Stop the runtime from complaining that this thread
                                             -- is blocked forever by creating a stable reference
                                             -- to this thread that could conceivably be thrown to.
  atomically retry -- Block forever.

last :: [a] -> Maybe a
last = lastMay

class (Ord (IndexOf a), Enum (IndexOf a)) =>
      Indexable a
  where
  type IndexOf a
  index :: a -> IndexOf a

moduleName ::
     forall t. Typeable t
  => Text
moduleName = pack . tyConModule . typeRepTyCon $ typeRep (Proxy :: Proxy t)

show :: Show a => a -> Text
-- ^ Redefine show to be text only.
-- If you want the old polymorphic behavior, use convertString . show
show = Protolude.show

getHostAndProcessId :: IO Text
getHostAndProcessId = do
  host <- getHostName
  CPid pid <- getProcessID
  return $ pack host <> ":" <> show pid
