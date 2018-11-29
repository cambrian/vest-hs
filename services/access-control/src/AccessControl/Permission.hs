module AccessControl.Permission
  ( T(..)
  , Is(..)
  ) where

import Vest

-- Use: import qualified AccessControl.Permission as Permission
data T
  = InvalidateAuthTokens
  | IssuePayout
  | ForgeOperation
  | B
  deriving (Eq, Ord, Enum, Read, Show, Generic, Hashable, ToJSON, FromJSON)

-- TODO: Can this boilerplate be inferred using GHC.Generics?
instance Is 'InvalidateAuthTokens where
  runtimeRep = InvalidateAuthTokens

instance Is 'IssuePayout where
  runtimeRep = IssuePayout

instance Is 'ForgeOperation where
  runtimeRep = ForgeOperation

instance Is 'B where
  runtimeRep = B

-- TODO: Replace HashSet T with list of types? singletons? Require (Typeable p)?
class Is p where
  runtimeRep :: T
