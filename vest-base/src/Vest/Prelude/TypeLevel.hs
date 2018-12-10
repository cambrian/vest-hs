{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Vest.Prelude.TypeLevel
  ( module Vest.Prelude.TypeLevel
  ) where

import Control.Applicative (liftA2)

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Semigroup (Semigroup(..))
import Data.Type.Bool as Vest.Prelude.TypeLevel
import GHC.Generics
import GHC.TypeLits as Vest.Prelude.TypeLevel (AppendSymbol)
import Vest.Prelude.Core

-- Type level alternative. Used to construct lists of heterogeneous types
data a :<|> b =
  a :<|> b
  deriving (Eq, Show, Functor, Traversable, Foldable, Bounded)

infixr 3 :<|>

instance (Semigroup a, Semigroup b) =>
         Semigroup (a
                    :<|> b) where
  (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) =>
         Monoid (a
                 :<|> b) where
  mempty = mempty :<|> mempty
  (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')

instance Bifoldable (:<|>) where
  bifoldMap f g ~(a :<|> b) = f a `mappend` g b

instance Bifunctor (:<|>) where
  bimap f g ~(a :<|> b) = f a :<|> g b

instance Bitraversable (:<|>) where
  bitraverse f g ~(a :<|> b) = liftA2 (:<|>) (f a) (g b)

symbolText :: (KnownSymbol a) => Proxy a -> Text
symbolText = pack . symbolVal

symbolText' :: (KnownSymbol a) => Proxy a -> Text' t
symbolText' = Tagged . symbolText

-- Checks if x in xs at type level.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- De-duplicates xs at type level.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = If (Elem x xs) (Nub xs) (x ': Nub xs)

-- Type-level array concatenation using :++.
type family (x :: [k]) :++ (y :: [k]) :: [k] where
  '[] :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

class SymbolTexts' (xs :: [Symbol]) where
  symbolTexts' :: Proxy xs -> [Text' t]

instance SymbolTexts' '[] where
  symbolTexts' _ = []

instance (KnownSymbol a, SymbolTexts' as) => SymbolTexts' (a ': as) where
  symbolTexts' _ =
    symbolText' (Proxy :: Proxy a) : symbolTexts' (Proxy :: Proxy as)

type family Symbols symbols where
  Symbols (Proxy c) = '[ c]
  Symbols (a
           :<|> b) = Symbols a :++ Symbols b

type family NubSymbols symbols where
  NubSymbols (Proxy c) = '[ c]
  NubSymbols (a
              :<|> b) = Nub (NubSymbols a :++ NubSymbols b)

type UniqueSymbols symbols = Symbols symbols ~ NubSymbols symbols

type family TypeName a :: Symbol where
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ())

type family ModuleName a :: Symbol where
  ModuleName (M1 D ('MetaData _ module_ _ _) f ()) = module_ -- module is a reserved symbol
  ModuleName a = ModuleName (Rep a ())

type FullTypeName a
   = AppendSymbol (ModuleName a) (AppendSymbol "." (TypeName a))
