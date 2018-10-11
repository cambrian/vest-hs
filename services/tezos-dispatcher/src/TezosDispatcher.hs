module TezosDispatcher
  ( module TezosDispatcher
  ) where

import Vest
import qualified Vest.Bridge.Transports.Amqp as Amqp

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
