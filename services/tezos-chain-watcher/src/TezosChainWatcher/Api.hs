module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Tezos
import TezosChainWatcher.Internal
import qualified Transport.Amqp as Amqp
import Vest

type CycleEvents = Event T Amqp.T "cycles" Tezos.CycleEvent

type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent

data RewardInfoRequest = RewardInfoRequest
  { cycleNumber :: Word64
  , delegates :: [Tezos.ImplicitAccount]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type RewardInfoEndpoint
   = Endpoint 'NoAuth T Amqp.T "rewardInfo" RewardInfoRequest ('Direct [Tezos.RewardInfo])
