-- Union of two APIs. Adapted from Servant.
module Vest.Prelude.Alternative
  ( (:<|>)(..)
  ) where

import Control.Applicative (liftA2)

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Semigroup (Semigroup(..))
import Data.Typeable (Typeable)
import Protolude

data a :<|> b =
  a :<|> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)

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
