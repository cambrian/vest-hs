module TezosDispatcher
  ( module TezosDispatcher
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import VestPrelude
import qualified VestPrelude.Money as Money

data TezosDispatcher = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  }

type instance ServiceArgs T = TezosDispatcher

instance Service T where
  defaultArgs = Args {}
  run _args f = with Amqp.localConfig (\amqp -> f $ T {amqp})
