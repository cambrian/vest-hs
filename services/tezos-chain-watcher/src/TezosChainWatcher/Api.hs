module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

data CycleEvent = CycleEvent
  { number :: Word64
  , timestamp :: Timestamp
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

type CycleEvents = Event T Amqp.T "cycles" CycleEvent

data Operation = Operation
  { hash :: Text' "OpHash"
  , sender :: Text' "Address"
  , recipient :: Maybe (Text' "Address")
  , size :: Maybe (FixedQty "XTZ")
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data BlockEvent = BlockEvent
  { number :: Word64
  , cycleNumber :: Word64
  , timestamp :: Timestamp
  , txFee :: FixedQty "XTZ"
  , operations :: [Operation]
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

type BlockEvents = Event T Amqp.T "blocks" BlockEvent
