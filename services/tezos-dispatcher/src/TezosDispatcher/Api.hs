module TezosDispatcher.Api
  ( module TezosDispatcher.Api
  ) where

import Bridge
import qualified Bridge.Rpc.Auth.Service as Auth.Service
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore
import VestPrelude
import qualified VestPrelude.Money as Money
-- type NotifyCycleEndpoint
--    = Endpoint ('Auth (Auth.Service.T todo)) "rewards" Db.Cycle ('Direct ())
-- type NotifyPayoutEndpoint
--    = Endpoint ('Auth (Auth.Service.T todo)) "notifyPayout" Db.Payout ('Direct ())
