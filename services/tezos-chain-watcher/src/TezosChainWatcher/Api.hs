module TezosChainWatcher.Api
  ( module TezosChainWatcher.Api
  ) where

import qualified Amqp
import qualified Tezos
import TezosChainWatcher.Internal
import Vest

data RewardInfoRequest = RewardInfoRequest
  { cycleNumber :: Word64
  , delegates :: [Tezos.ImplicitAddress]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

type RewardInfoEndpoint
   = Endpoint 'NoAuth T Amqp.T "rewardInfo" RewardInfoRequest ('Direct (Either Tezos.InvalidCycleException [Tezos.RewardInfo]))

type MonitorOperationEndpoint
   = Endpoint_ 90 'Haskell 'NoAuth T Amqp.T "monitorOperation" Tezos.OperationHash ('Streaming Tezos.OperationStatus)

type ProvisionalHeightValue = ValueTopic T Amqp.T "provisionalHeight" Word64

type FinalizedHeightValue = ValueTopic T Amqp.T "finalizedHeight" Word64

type OperationFeeValue = ValueTopic T Amqp.T "operationFee" (FixedQty XTZ)

type FinalizedBlockEvents = Event T Amqp.T "finalizedBlock" Tezos.BlockEvent

withMonitor ::
     Amqp.T -> Tezos.OperationHash -> (Tezos.OperationStatus -> IO ()) -> IO ()
withMonitor amqp opHash =
  let monitorClient = makeClient amqp (Proxy :: Proxy MonitorOperationEndpoint)
   in ($) monitorClient opHash . consumeStream
