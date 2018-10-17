module Vest.Prelude.Money
  ( module Vest.Prelude.Money
  ) where

import Money
import Money.Aeson ()
import Vest.Prelude.Core

-- use:
-- import Vest
-- import qualified Money
-- (this refers to the external Money package)
type instance Scale "XTZ" "XTZ" = '(1000000, 1)

type instance Scale "XTZ" "tezos" = '(1, 1)

type instance Scale "XTZ" "mutez" = '(1000000, 1)

type instance Scale "XTZ" "roll" = '(1, 10000)

type family CurrencyUnit currency where
  CurrencyUnit "USD" = "cent"
  CurrencyUnit "XTZ" = "mutez"

type FixedQty currency = Discrete currency (CurrencyUnit currency)

type FixedQty' tag currency = Tagged tag (FixedQty currency)

type RationalQty currency = Dense currency

type RationalQty' tag currency = Tagged tag (RationalQty currency)

class ( KnownSymbol currency
      , KnownSymbol (CurrencyUnit currency)
      , GoodScale (Scale currency (CurrencyUnit currency))
      ) =>
      Currency currency


instance ( KnownSymbol currency
         , KnownSymbol (CurrencyUnit currency)
         , GoodScale (Scale currency (CurrencyUnit currency))
         ) =>
         Currency currency
