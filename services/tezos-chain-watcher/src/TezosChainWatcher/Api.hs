module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Tezos
import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

-- We switch to Error state and end the stream after X number of blocks.
data MonitorOpResponse = MonitorOpResponse
  { status :: OpStatus
  , confirmations :: Word64
  , blockHash :: Maybe Tezos.BlockHash
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

-- Eventually: Make this endpoint batch.
type MonitorOpEndpoint
   = Endpoint 'NoAuth T Amqp.T "monitorOp" Tezos.OperationHash ('Streaming MonitorOpResponse)

data RewardInfoRequest = RewardInfoRequest
  { cycleNumber :: Word64
  , delegates :: [Tezos.ImplicitAddress]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type RewardInfoEndpoint
   = Endpoint 'NoAuth T Amqp.T "rewardInfo" RewardInfoRequest ('Direct [Tezos.RewardInfo])

type OriginatedMappingEndpoint
   = Endpoint 'NoAuth T Amqp.T "originatedMapping" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])

type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent

type CycleEvents = Event T Amqp.T "cycles" Tezos.CycleEvent
