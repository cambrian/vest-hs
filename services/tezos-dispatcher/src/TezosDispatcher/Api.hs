module TezosDispatcher.Api
  ( module TezosDispatcher.Api
  ) where

import Bridge
import qualified Bridge.Rpc.Auth.Service as Auth.Service
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore
import qualified TezosDispatcher
import VestPrelude
import qualified VestPrelude.Money as Money

type RewardsEndpoint = Endpoint 'NoAuth "getRewards" () ('Direct ())
