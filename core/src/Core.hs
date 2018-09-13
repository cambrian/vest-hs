module Core
  (
  ) where

import VestPrelude
import qualified VestPrelude.Money as Money

data ContractT f = Contract
  { _contractOwner :: Columnar f Text
  } deriving (Generic)
