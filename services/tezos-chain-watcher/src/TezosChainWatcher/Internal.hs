module TezosChainWatcher.Internal
  ( module TezosChainWatcher.Internal
  ) where

import qualified AccessControl.Client
import qualified Db
import qualified Http
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { db :: Db.Connection
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezos :: Http.T
  , accessControlClient :: AccessControl.Client.T
  , reachedSteadyState :: TMVar ()
  } deriving (HasNamespace)

instance HasRedisConnection T where
  redisConnection = redis

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasEventTransport Amqp.T T where
  eventTransport = amqp

instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient

instance HasLogger T where
  logger _ = stderrLogger Warn
