module TezosDelegationCore.Api
  ( module TezosDelegationCore.Api
  ) where

import Bridge
import qualified Bridge.Rpc.Auth.Service as Auth.Service
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore.Db as Db
import qualified TezosDispatcher
import VestPrelude
import qualified VestPrelude.Money as Money

type NotifyCycleEndpoint
   = Endpoint ('Auth (Auth.Service.T TezosDispatcher.T)) "notifyCycle" Db.Cycle ('Direct ())

type NotifyPayoutEndpoint
   = Endpoint ('Auth (Auth.Service.T TezosDispatcher.T)) "notifyPayout" Db.Payout ('Direct ())
