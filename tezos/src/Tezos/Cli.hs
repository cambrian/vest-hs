module Tezos.Cli
  ( module Tezos.Cli
  ) where

import Tezos.Prelude
import Vest

-- This should probably have one more parameter in front, which either specifies our keys or some
-- config information to extract keys from hardware/filesystem.
makePayout :: Address -> FixedQty XTZ -> IO OperationHash
makePayout _recipient _size = panic "unimplemented"
