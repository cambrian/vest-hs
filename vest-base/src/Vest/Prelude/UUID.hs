module Vest.Prelude.UUID
  ( module Reexports
  , UUID'
  , nextUUID'
  ) where

import Data.Aeson.TypeScript.TH
import Data.UUID as Reexports (UUID)
import qualified Data.UUID.V4 as UUID.V4
import GHC.TypeLits
import Vest.Prelude.Core

type UUID' t = Tagged t UUID

nextUUID' :: IO (UUID' t)
nextUUID' = UUID.V4.nextRandom >>- Tagged

-- UUIDs are tagged text, but we tack on Id to disambiguate the tagging.
instance {-# OVERLAPPING #-} (KnownSymbol (AppendSymbol s "Id")) =>
                             TypeScript (UUID' (s :: Symbol)) where
  getTypeScriptType _ =
    getTypeScriptType (Proxy :: Proxy (Text' (AppendSymbol s "Id")))
  getTypeScriptDeclarations _ =
    getTypeScriptDeclarations (Proxy :: Proxy (Text' (AppendSymbol s "Id")))
