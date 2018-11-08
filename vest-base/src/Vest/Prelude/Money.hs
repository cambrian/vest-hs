-- TODO: consider replacing safe-money with units for better interop with other dimensional types
module Vest.Prelude.Money
  ( module Vest.Prelude.Money
  ) where

import Money
import Money as Vest.Prelude.Money (Approximation(..))
import Money.Aeson ()
import Vest.Prelude.Core

-- Currency aliases
type USD = "USD"

type XTZ = "XTZ"

-- Currency unit aliases
type Cent = "cent"

type Mutez = "mutez"

type instance Scale XTZ XTZ = '(1, 1)

type instance Scale XTZ "tezos" = '(1, 1)

type instance Scale XTZ "mutez" = '(1000000, 1)

type instance Scale XTZ "roll" = '(1, 10000)

type family CurrencyUnit currency where
  CurrencyUnit USD = "cent"
  CurrencyUnit XTZ = "mutez"

type FixedQty currency = Discrete currency (CurrencyUnit currency)

type FixedQty' tag currency = Tagged tag (FixedQty currency)

type RationalQty currency = Dense currency

type RationalQty' tag currency = Tagged tag (RationalQty currency)

rationalOf :: Currency currency => FixedQty currency -> RationalQty currency
rationalOf = denseFromDiscrete

fixedOf ::
     Currency currency
  => Approximation
  -> RationalQty currency
  -> FixedQty currency
fixedOf approx a = fst $ discreteFromDense approx a

fixedOfRem ::
     Currency currency
  => Approximation
  -> RationalQty currency
  -> (FixedQty currency, RationalQty currency)
-- ^ Converts rational qty to (fixed qty, remainder)
fixedOfRem = discreteFromDense

rationalQty :: Rational -> Dense currency
rationalQty = dense'

fixedQty ::
     forall unit currency. GoodScale (Scale currency unit)
  => Integer
  -> Discrete currency unit
fixedQty = discrete

instance VectorDivisible (RationalQty currency) where
  a ^/^ b = toRational a / toRational b

instance GoodScale scale => VectorDivisible (Discrete' currency scale) where
  a ^/^ b = toInteger a % toInteger b

-- instance VectorSpace Discrete (currency CurrencyUnit currency) where
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
