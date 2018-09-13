module VestPrelude.Money
  ( module VestPrelude.Money
  , module Money
  , module Money.Aeson
  ) where

import Money
import Money.Aeson ()
import Protolude

type instance Scale "XTZ" "XTZ" = '(1000000, 1)

type instance Scale "XTZ" "tezos" = '(1, 1)

type instance Scale "XTZ" "mutez" = '(1000000, 1)

type instance Scale "XTZ" "roll" = '(1, 10000)
