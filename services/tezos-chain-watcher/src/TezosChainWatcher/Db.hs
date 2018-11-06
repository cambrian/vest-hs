module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Db
import qualified Tezos.Prelude as Tezos
import Vest

data BlockT f = Block
  { number :: C f Word64
  , hash :: C f Tezos.BlockHash
  , cycleNumber :: C f Word64
  , fee :: C f (FixedQty XTZ)
  , timestamp :: C f Timestamp
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockNumber (C f Word64)
                             deriving (Generic, Beamable)
  primaryKey Block {number} = BlockNumber number

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

data OperationKind
  = OriginationKind
  | TransactionKind
  | OtherKind
  deriving (Eq, Read, Show)

data OperationT f = Operation
  { hash :: C f Tezos.OperationHash
  , blockNumber :: PrimaryKey BlockT f
  , kind :: C f OperationKind
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationHash (C f
                                                Tezos.OperationHash)
                                 deriving (Generic, Beamable)
  primaryKey Operation {hash} = OperationHash hash

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data OriginationT f = Origination
  { opHash :: PrimaryKey OperationT f
  , originator :: C f Tezos.ImplicitAccount
  , originated :: C f Tezos.OriginatedAccount
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Origination = OriginationT Identity

deriving instance Eq Origination

deriving instance Read Origination

deriving instance Show Origination

instance Table OriginationT where
  data PrimaryKey OriginationT f = OriginationOpHash (PrimaryKey
                                                      OperationT
                                                      f)
                                   deriving (Generic, Beamable)
  primaryKey Origination {opHash} = OriginationOpHash opHash

deriving instance Eq (PrimaryKey OriginationT Identity)

deriving instance Read (PrimaryKey OriginationT Identity)

deriving instance Show (PrimaryKey OriginationT Identity)

data TransactionT f = Transaction
  { opHash :: PrimaryKey OperationT f
  , from :: C f Tezos.Account
  , to :: C f Tezos.Account
  , size :: C f (FixedQty XTZ)
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Transaction = TransactionT Identity

deriving instance Eq Transaction

deriving instance Read Transaction

deriving instance Show Transaction

instance Table TransactionT where
  data PrimaryKey TransactionT f = TransactionOpHash (PrimaryKey
                                                      OperationT
                                                      f)
                                   deriving (Generic, Beamable)
  primaryKey Transaction {opHash} = TransactionOpHash opHash

deriving instance Eq (PrimaryKey TransactionT Identity)

deriving instance Read (PrimaryKey TransactionT Identity)

deriving instance Show (PrimaryKey TransactionT Identity)

data T f = T
  { blocks :: f (TableEntity BlockT)
  , operations :: f (TableEntity OperationT)
  , originations :: f (TableEntity OriginationT)
  , transactions :: f (TableEntity TransactionT)
  } deriving (Generic)

schema :: DatabaseSettings be T
schema = defaultDbSettings
