module Tezos.Prelude
  ( module Tezos.Prelude
  ) where

import Vest

type BlockHash = Text' "TzBlockHash"

type OperationHash = Text' "TzOperationHash"

type ImplicitPkh = Text' "TzImplicitPkh"

type OriginatedHash = Text' "TzOriginatedHash"

type AccountHash = Text' "TzAccountHash" -- Either type of account hash.

type Operation = Text' "TzOperation"
