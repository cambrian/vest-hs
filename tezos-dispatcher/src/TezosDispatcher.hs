module TezosDispatcher
  ( module TezosDispatcher
  ) where

import Bridge
import qualified Bridge.Transports.Amqp as Amqp
import TezosDispatcher.Api
import VestPrelude
import qualified VestPrelude.Money as Money

data Args = Args
  {
  } deriving (Eq, Show, Read, Generic, Data)

data T = T
  { amqp :: Amqp.T
  }

type instance ServiceArgs T = Args

instance Service T where
  defaultArgs = Args {}
  start T {amqp} = blockForever
  withResources args start = with Amqp.localConfig (\amqp -> start $ T {amqp})
