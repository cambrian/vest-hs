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

type MonitorOperationEndpoint
   = Endpoint 'NoAuth T Amqp.T "monitorOperation" (Tezos.OperationHash, Word8) ('Streaming Tezos.OperationStatus)

type FinalizedHeightValue = ValueTopic T Amqp.T "finalizedHeight" Word64

-- TODO: Write.
type OperationFeeValue = ValueTopic T Amqp.T "OperationFee" (FixedQty XTZ)

-- | This refers specifically to final block events (confirmed 60 times).
type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent
