module Tezos.Cli
  ( module Tezos.Cli
  ) where

import Tezos.Prelude
import Vest

data T = T
  { cliPath :: FilePath
  , addressSecret :: AddressSecret
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance Loadable T where
  configFile = [relfile|tezos-cli.yaml|]

makePayout :: T -> Address -> FixedQty XTZ -> IO OperationHash
makePayout _t _recipient _size = panic "unimplemented"
