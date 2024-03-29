module TezosPlatformCore.Internal
  ( module TezosPlatformCore.Internal
  ) where

import qualified AccessControl.Client
import qualified Amqp
import qualified Postgres
import Vest

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , accessControlClient :: AccessControl.Client.T
  , operationFee :: Stream ValueBuffer (FixedQty XTZ)
  , platformFee :: Rational
  }

instance HasNamespace T where
  type Namespace T = "tezos-platform-core"

instance Has (Pool (Specific T Postgres.Connection)) T where
  get = dbPool

instance Has Amqp.T T where
  get = amqp

instance Has RedisConnection T where
  get = redis

instance Has AccessControl.Client.T T where
  get = accessControlClient
