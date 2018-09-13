module VestPrelude.Money
  ( module VestPrelude.Money
  , module Money
  ) where

-- use: import qualified VestPrelude.Money as Money
import Data.Aeson (FromJSON, ToJSON)
import Money
import Protolude

type instance Scale "XTZ" "XTZ" = '(1000000, 1)

type instance Scale "XTZ" "tezos" = '(1, 1)

type instance Scale "XTZ" "mutez" = '(1000000, 1)

type instance Scale "XTZ" "roll" = '(1, 10000)
