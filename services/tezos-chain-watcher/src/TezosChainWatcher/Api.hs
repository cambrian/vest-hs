module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Tezos
import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

-- Writing this explicitly causes an auto-format shitshow.
type OpBlockInfoResponse = Maybe (Word64, Tezos.BlockHash)

-- Eventually: Some kind of built-in endpoint batching?
type OpBlockInfoEndpoint
   = Endpoint_ 60 'Haskell 'NoAuth T Amqp.T "opBlockInfo" Tezos.OperationHash ('Direct OpBlockInfoResponse)

data RewardInfoRequest = RewardInfoRequest
  { cycleNumber :: Word64
  , delegates :: [Tezos.ImplicitAddress]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type RewardInfoEndpoint
   = Endpoint 'NoAuth T Amqp.T "rewardInfo" RewardInfoRequest ('Direct [Tezos.RewardInfo])

type ImplicitOriginationsEndpoint
   = Endpoint 'NoAuth T Amqp.T "implicitOriginations" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])

type LatestBlockEventValue
   = ValueTopic T Amqp.T "latestBlockEvent" Tezos.BlockEvent

type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent

type ProvisionalBlockEvents
   = Event T Amqp.T "provisionalBlocks" Tezos.BlockEvent
