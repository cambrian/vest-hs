module TezosDispatcher
  ( module TezosDispatcher
  ) where

import qualified Transports.Amqp as Amqp
import Vest

data TezosDispatcher = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  }

type RewardsEndpoint
   = Endpoint 'NoAuth TezosDispatcher.T "getRewards" () ('Direct ())

instance Service T where
  type ServiceArgs T = TezosDispatcher
  type RpcSpec T = ()
  type PubSubSpec T = ()
  defaultArgs = Args {}
  init _args f = with Amqp.localConfig (\amqp -> f $ T {amqp})
