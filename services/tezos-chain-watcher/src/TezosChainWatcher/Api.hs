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

type ImplicitOriginationsEndpoint
   = Endpoint 'NoAuth T Amqp.T "implicitOriginations" Tezos.ImplicitAddress ('Direct [Tezos.OriginatedAddress])

type LatestBlockEventValue
   = ValueTopic T Amqp.T "latestBlockEvent" Tezos.BlockEvent

type BlockEvents = Event T Amqp.T "blocks" Tezos.BlockEvent

type ProvisionalBlockEvents
   = Event T Amqp.T "provisionalBlocks" ProvisionalEvent

instance Indexable ProvisionalEvent where
  type IndexOf ProvisionalEvent = Int
  index = fst
