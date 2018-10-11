module TezosDispatcher.Api
  ( module TezosDispatcher.Api
  ) where

import qualified TezosDelegationCore
import qualified TezosDispatcher
import Vest
import Vest.Bridge
import qualified Vest.Bridge.Transports.Amqp as Amqp
import qualified Vest.Money as Money

type RewardsEndpoint
   = Endpoint TezosDispatcher.T 'NoAuth "getRewards" () ('Direct ())
