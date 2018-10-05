module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import qualified TezosDelegationCore.Db as Db
import VestPrelude
import qualified VestPrelude.Money as Money

data TezosDelegationCore = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

dbPoolConfig :: PoolConfig
dbPoolConfig = PoolConfig {idleTime = sec 30, numResources = 5}

data T = T
  { amqp :: Amqp.T
  , dbPool :: Pool Db.Connection
  }

type instance ServiceArgs T = TezosDelegationCore

instance Service T where
  defaultArgs = Args {}
  run _args f =
    with
      Amqp.localConfig
      (\amqp ->
         withPool dbPoolConfig Db.localConfig (\dbPool -> f $ T {amqp, dbPool}))
