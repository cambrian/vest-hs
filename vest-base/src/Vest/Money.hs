module Vest.Money
  ( module Vest.Money
  ) where

import Money
import Money.Aeson ()
import Vest.Prelude

type instance Scale "XTZ" "XTZ" = '(1000000, 1)

type instance Scale "XTZ" "tezos" = '(1, 1)

type instance Scale "XTZ" "mutez" = '(1000000, 1)

type instance Scale "XTZ" "roll" = '(1, 10000)

instance VestCurrency "USD" "cent"

instance VestCurrency "XTZ" "mutez"

type USD = Discrete "USD" "cent"

type XTZ = Discrete "XTZ" "mutez"

-- Put empty class at bottom of file because it messes up syntax highlighting.
class (KnownSymbol currency, KnownSymbol unit, GoodScale (Scale currency unit)) =>
      VestCurrency currency unit
