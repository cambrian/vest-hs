module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import TezosDelegationCore.Api
import qualified TezosDelegationCore.Db as Db
import VestPrelude
import qualified VestPrelude.Money as Money

data Args = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

dbPoolConfig :: PoolConfig
dbPoolConfig = PoolConfig {idleTime = sec 30, numResources = 5}

data T = T
  { amqp :: Amqp.T
  , dbPool :: Pool Db.Connection
  }

type instance ServiceArgs T = Args

instance Service T where
  defaultArgs = Args {}
  start T {amqp, dbPool} = blockForever
  withResources args start =
    with
      Amqp.localConfig
      (\amqp ->
         withPool
           dbPoolConfig
           Db.localConfig
           (\dbPool -> start $ T {amqp, dbPool}))
