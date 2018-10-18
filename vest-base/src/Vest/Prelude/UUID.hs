module Vest.Prelude.UUID
  ( module Reexports
  , UUID'
  , nextUUID'
  ) where

import Data.UUID as Reexports (UUID)
import qualified Data.UUID.V4 as UUID.V4
import Vest.Prelude.Core

type UUID' t = Tagged t UUID

nextUUID' :: IO (UUID' t)
nextUUID' = UUID.V4.nextRandom >>- Tagged
