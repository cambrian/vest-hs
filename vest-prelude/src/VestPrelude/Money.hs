module VestPrelude.Money
  ( module VestPrelude.Money
  , module Reexports
  ) where

import Money as Reexports
import Money.Aeson as Reexports ()

-- Use: import qualified VestPrelude.Money as Money.
import VestPrelude

type instance Scale "XTZ" "XTZ" = '(1000000, 1)

type instance Scale "XTZ" "tezos" = '(1, 1)

type instance Scale "XTZ" "mutez" = '(1000000, 1)

type instance Scale "XTZ" "roll" = '(1, 10000)
