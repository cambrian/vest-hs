module VestPrelude
  ( module Protolude
  , Id(..)
  , Route(..)
  ) where

import Protolude

-- If you want the Id or Route string you should destructure instead of using show:
-- show (Id "x") == "Id \"x\""
-- let Id text = (Id "x") -> text == "x"
newtype Id =
  Id Text
  deriving (Eq, Ord, Show, Read, Typeable, Hashable)

newtype Route =
  Route Text
  deriving (Eq, Ord, Show, Read, Typeable, Hashable)
