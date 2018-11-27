module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Tezos
import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

data RewardInfoRequest = RewardInfoRequest
  { cycleNumber :: Word64
  , delegates :: [Tezos.ImplicitAddress]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type RewardInfoEndpoint
   = Endpoint 'NoAuth T Amqp.T "rewardInfo" RewardInfoRequest ('Direct [Tezos.RewardInfo])

type HighestSeenBlockNumberValue
   = ValueTopic T Amqp.T "highestSeenBlockNumber" Word64

-- | This refers specifically to final block events (confirmed 60 times).
type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent

-- | The Word8 field is the number of blocks for which the operation has been in a given state
-- (i.e. confirmations of that state).
data OperationStatus
  = NotIncluded Word8
  | Included (Tezos.BlockHash, Word8)
  | Confirmed Tezos.BlockHash
  | Error -- Add a Text for error message?
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type MonitorOperationEndpoint
   = Endpoint 'NoAuth T Amqp.T "monitorOperation" Tezos.OperationHash ('Streaming OperationStatus)
