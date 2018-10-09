module TezosDispatcher
  ( module TezosDispatcher
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import Vest
import qualified Vest.Money as Money

data TezosDispatcher = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  }

instance Service T where
  type ServiceArgs T = TezosDispatcher
  type RpcSpec T = ()
  type PubSubSpec T = ()
  defaultArgs = Args {}
  init _args f = with Amqp.localConfig (\amqp -> f $ T {amqp})
