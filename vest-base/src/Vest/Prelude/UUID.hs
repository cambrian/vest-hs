module Vest.Prelude.UUID
  ( module Vest.Prelude.UUID
  ) where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Vest.Prelude.Core

-- TODO: would it be better to work with the UUID type directly??
newUUID :: IO (Text' t)
newUUID = UUID.nextRandom >>- (Tagged . UUID.toText)
