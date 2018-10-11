module Vest.Prelude.Money
  ( module Vest.Prelude.Money
  ) where

import Money
import Vest.Prelude.Core

-- use:
-- import Vest
-- import qualified Money
-- (this refers to the external Money package)
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
