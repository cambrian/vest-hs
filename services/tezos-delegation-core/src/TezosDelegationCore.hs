module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import qualified TezosDelegationCore.Db as Db
import Vest
import qualified Vest.Bridge.Transports.Amqp as Amqp

import AccessControl (PubKeyEndpoint)

data TezosDelegationCore = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

dbPoolConfig :: PoolConfig
dbPoolConfig = PoolConfig {idleTime = sec 30, numResources = 5}

data T = T
  { amqp :: Amqp.T
  , dbPool :: Pool Db.Connection
  }

type NotifyCycleEndpoint
   = Endpoint TezosDelegationCore.T 'NoAuth "notifyCycle" Db.Cycle ('Direct ())

type NotifyPayoutEndpoint
   = Endpoint TezosDelegationCore.T 'NoAuth "notifyPayout" Db.Payout ('Direct ())


instance Service T where
  type ServiceArgs T = TezosDelegationCore
  type RpcSpec T = ()
  type PubSubSpec T = ()
  defaultArgs = Args {}
  init _args f =
    with
      Amqp.localConfig
      (\amqp ->
         withPool dbPoolConfig Db.localConfig (\dbPool -> f $ T {amqp, dbPool}
         getAccessControlPubKey = makeClient PubKeyEndpoint))
