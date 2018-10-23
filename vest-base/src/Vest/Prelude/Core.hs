-- Module for super base things. This file should be mostly reexports
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
import Data.HashMap.Strict as Vest.Prelude.Core (HashMap)
import Data.HashSet as Vest.Prelude.Core (HashSet)
import Data.Hashable as Vest.Prelude.Core (Hashable(..))
import Data.Proxy as Vest.Prelude.Core
import Numeric.Natural as Vest.Prelude.Core
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
  , mask
  , mask_
  , onException
  , threadDelay
  , throwIO
  , throwTo
  , try
  , tryIO
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  )

-- import qualified Control.Monad.Fail
import Control.Monad.Fail as Vest.Prelude.Core
import Control.Monad.State as Vest.Prelude.Core hiding (fail)
import Data.Aeson as Vest.Prelude.Core
  ( FromJSON(..)
  , FromJSONKey(..)
  , ToJSON(..)
  , ToJSONKey(..)
  )
import Data.Aeson (withText)
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import qualified Data.ByteString.Base64 as Base64
import Data.Data as Vest.Prelude.Core (Data(..))
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
import qualified Foreign.StablePtr as StablePtr
import System.Random as Vest.Prelude.Core

type Int' t = Tagged t Int

type Int64' t = Tagged t Int64

type Text' t = Tagged t Text

type IO' t a = Tagged t (IO a)

type STM' t a = Tagged t (STM a)

type Async' t a = Tagged t (Async a)

async' :: IO a -> IO (Async' t a)
async' x = Tagged <$> async x

instance Hashable a => Hashable (Tagged s a)

data BugException =
  BugException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

-- Aeson instances copied from base64-bytestring-type.
-- ByteStrings are serialized as Latin-1 (ASCII) Texts, after base64 encoding.
instance ToJSON ByteString where
  toJSON = toJSON . decodeLatin1 . Base64.encode
  toEncoding = toEncoding . decodeLatin1 . Base64.encode

instance FromJSON ByteString where
  parseJSON =
    withText "ByteString" $ either fail pure . Base64.decode . encodeUtf8

instance ToJSONKey ByteString where
  toJSONKey = toJSONKeyText (decodeLatin1 . Base64.encode)

instance FromJSONKey ByteString where
  fromJSONKey =
    FromJSONKeyTextParser $ either fail pure . Base64.decode . encodeUtf8

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

-- Infix map, like (>>=) but pure.
infixl 1 >>-

(>>-) :: (Functor f) => f a -> (a -> b) -> f b
(>>-) ma f = map f ma

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
