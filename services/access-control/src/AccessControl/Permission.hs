module AccessControl.Permission
  ( T(..)
  , Is(..)
  ) where

import Vest

-- Use: import qualified AccessControl.Permission as Permission
data T
  = InvalidateAuthTokens
  | B
  deriving (Eq, Ord, Read, Show, Generic, Hashable, ToJSON, FromJSON)

-- TODO: can this boilerplate be inferred??
-- Probably, using GHC.Generics. Figure that out at some pt.
instance Is 'InvalidateAuthTokens where
  runtimeRep = InvalidateAuthTokens

instance Is 'B where
  runtimeRep = B

-- TODO: replace HashSet T with list of types? singletons? require (Typeable p)?
class Is p where
  runtimeRep :: T
