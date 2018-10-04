module TezosDelegationCore
  ( module TezosDelegationCore
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import TezosDelegationCore.Api
import qualified TezosDelegationCore.Db as Db
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  { dbPool :: Pool Db.T
  }

type instance ResourceConfig T = (Config, Amqp.T, Pool Db.T)

instance Resource T where
  make :: (Config, Amqp.T, Pool Db.T) -> IO T
  make (_config, amqp, dbPool) = do
    return T {dbPool}
  cleanup :: T -> IO ()
  cleanup _t = panic "unimplemented"

start :: Amqp.T -> T -> IO ()
start rpcTransport t = return ()
