module TezosDispatcher.Api
  ( module TezosDispatcher.Api
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore
import qualified TezosDispatcher
import Vest
import qualified Vest.Money as Money

type RewardsEndpoint
   = Endpoint TezosDispatcher.T 'NoAuth "getRewards" () ('Direct ())
