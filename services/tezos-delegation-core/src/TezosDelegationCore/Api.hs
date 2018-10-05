module TezosDelegationCore.Api
  ( module TezosDelegationCore.Api
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore
import qualified TezosDelegationCore.Db as Db
import qualified TezosDispatcher
import VestPrelude
import qualified VestPrelude.Money as Money

-- TODO: authenticate
type NotifyCycleEndpoint
   = Endpoint TezosDelegationCore.T 'NoAuth "notifyCycle" Db.Cycle ('Direct ())

type NotifyPayoutEndpoint
   = Endpoint TezosDelegationCore.T 'NoAuth "notifyPayout" Db.Payout ('Direct ())
