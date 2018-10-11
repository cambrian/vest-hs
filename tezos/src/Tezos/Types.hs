module Tezos.Types
  ( module Tezos.Types
  ) where

import Servant.API
import Vest

data Block = Block
  { hash :: Text
  , level :: Int
  , proto :: Int
  , predecessor :: Text
  , timestamp :: Text
  , validation_pass :: Int
  , operations_hash :: Text
  , fitness :: [Text]
  , context :: Text
  , protocol_data :: Text
  } deriving (Show, Generic, FromJSON)

type MonitorBlocks
   = "monitor" :> "heads" :> "main" :> StreamGet NewlineFraming JSON (ResultStream Block)
