module Core
  (
  ) where

import Database.Beam
import VestPrelude
import qualified VestPrelude.Money as Money

data ContractT f = Contract
  { _contractOwner :: Columnar f Text
  } deriving (Generic)
