-- Module for super base things. This file should be mostly reexports
module Vest.Prelude.Core
  ( module Vest.Prelude.Core
  ) where

import Control.Concurrent.Async as Vest.Prelude.Core
import Control.Concurrent.MVar as Vest.Prelude.Core
import Control.Concurrent.STM.Delay as Vest.Prelude.Core
import Control.Concurrent.STM.TVar as Vest.Prelude.Core
import qualified Control.Exception as Evil (Exception, throwTo)
import Control.Exception.Safe as Vest.Prelude.Core
import Control.Monad.STM as Vest.Prelude.Core
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

import Control.Monad.Fail (fail)
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
import Data.Tagged as Vest.Prelude.Core hiding (witness)
import Data.Text as Vest.Prelude.Core (pack, unpack)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Foreign.StablePtr as StablePtr

type Int' t = Tagged t Int

type Int64' t = Tagged t Int64

type Text' t = Tagged t Text

type IO' t a = Tagged t (IO a)

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

-- DO NOT USE unless you really really know what you're doing.
evilThrowTo :: (Evil.Exception e) => ThreadId -> e -> IO ()
evilThrowTo = Evil.throwTo

fromJustUnsafe :: (Exception e) => e -> Maybe a -> IO a
fromJustUnsafe e Nothing = throw e
fromJustUnsafe _ (Just a) = return a

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
