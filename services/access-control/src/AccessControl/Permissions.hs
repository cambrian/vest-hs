module AccessControl.Permissions
  ( module AccessControl.Permissions
  ) where

import Vest

data Permission
  = A
  | B
  deriving (Eq, Read, Show, Generic, Hashable, ToJSON, FromJSON)

-- TODO: can this boilerplate be inferred??
-- Probably, using GHC.Generics. Figure that out at some pt.
instance IsPermission 'A where
  runtimeRep = A

instance IsPermission 'B where
  runtimeRep = B

class IsPermission p where
  runtimeRep :: Permission
