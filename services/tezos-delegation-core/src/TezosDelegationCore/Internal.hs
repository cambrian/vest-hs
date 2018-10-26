module TezosDelegationCore.Internal
  ( module TezosDelegationCore.Internal
  ) where

import qualified AccessControl.Client as AccessControlClient
import qualified Db
import qualified Transport.Amqp as Amqp
import Vest

data T = T
  { db :: Db.Connection
  , amqp :: Amqp.T
  , accessControlClient :: AccessControlClient.T
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance AccessControlClient.Has T where
  accessControlClient = accessControlClient
