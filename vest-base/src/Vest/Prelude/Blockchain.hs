module Vest.Prelude.Blockchain
  ( module Vest.Prelude.Blockchain
  ) where

import Vest.Prelude.Core

data TxStatus
  = Pending
  | Confirmed
  | Rejected
  deriving (Eq, Ord, Read, Show, Enum, Generic, ToJSON, FromJSON, Hashable)