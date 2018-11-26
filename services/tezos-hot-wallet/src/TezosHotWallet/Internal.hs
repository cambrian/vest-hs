module TezosHotWallet.Internal
  ( module TezosHotWallet.Internal
  ) where

import qualified AccessControl.Client
import qualified Postgres
import qualified Tezos
import qualified Tezos.Cli
import qualified Transport.Amqp as Amqp
import Vest

data PaymentEvent = PaymentEvent
  { idx :: Word64
  , hash :: Tezos.OperationHash
  , from :: Tezos.Address
  , size :: FixedQty XTZ
  , time :: Time
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable PaymentEvent where
  type IndexOf PaymentEvent = Word64
  index = idx

data T = T
  { dbPool :: Pool (Specific T Postgres.Connection)
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , tezosCli :: Tezos.Cli.T
  , accessControlClient :: AccessControl.Client.T
  , paymentWriter :: StreamWriter PaymentEvent
  , paymentStream :: Stream QueueBuffer PaymentEvent
  , newPaymentVar :: TMVar ()
  }

instance HasNamespace T where
  type Namespace T = "tezos-hot-wallet"

instance Has (Pool (Specific T Postgres.Connection)) T where
  get = dbPool

instance Has Amqp.T T where
  get = amqp

instance Has RedisConnection T where
  get = redis

instance Has AccessControl.Client.T T where
  get = accessControlClient
