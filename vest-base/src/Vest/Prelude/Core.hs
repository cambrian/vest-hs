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
import Protolude as Vest.Prelude.Core hiding
  ( Fixity
  , Infix
  , Prefix
  , bracket
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
  , throwIO
  , throwTo
  , try
  , tryIO
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  , witness
  )

import Data.Aeson as Vest.Prelude.Core (FromJSON, ToJSON)
import Data.Data as Vest.Prelude.Core
import Data.Tagged as Vest.Prelude.Core
import Data.Text as Vest.Prelude.Core (pack, unpack)
import qualified Foreign.StablePtr as StablePtr
import GHC.TypeLits as Vest.Prelude.Core (AppendSymbol)

type Int' t = Tagged t Int

type Int64' t = Tagged t Int64

type Text' t = Tagged t Text

type IO' t a = Tagged t (IO a)

instance Hashable a => Hashable (Tagged s a)

data BugException =
  BugException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

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

proxyText' :: (KnownSymbol a) => Proxy a -> Text' t
proxyText' = Tagged . pack . symbolVal

blockForever :: IO Void
blockForever = do
  _ <- myThreadId >>= StablePtr.newStablePtr -- Stop the runtime from complaining that this thread
                                             -- is blocked forever by creating a stable reference
                                             -- to this thread that could conceivably be thrown to.
  atomically retry -- Block forever.
