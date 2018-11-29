module TezosInjector.Db
  ( module TezosInjector.Db
  ) where

import Postgres
import qualified Tezos
import Vest hiding (from, hash, to)

data OperationT f = Operation
  { operationSignedBytes :: C f Tezos.SignedOperation
  , operationObject :: C f Tezos.OperationObject
  , operationHash :: C f (Maybe Tezos.OperationHash)
  , operationCreatedAt :: C f Time
  , operationUpdatedAt :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationSignedBytes (C f
                                                       Tezos.SignedOperation)
                                 deriving (Generic, Beamable)
  primaryKey Operation {operationSignedBytes} =
    OperationSignedBytes operationSignedBytes

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data ConstantT f = Constant
  { key :: C f Text
  , value :: C f Text
  , createdAt :: C f Time
  } deriving (Generic, Beamable)

type Constant = ConstantT Identity

deriving instance Eq Constant

deriving instance Read Constant

deriving instance Show Constant

instance Table ConstantT where
  data PrimaryKey ConstantT f = ConstantKey (C f Text)
                                deriving (Generic, Beamable)
  primaryKey Constant {key} = ConstantKey key

deriving instance Eq (PrimaryKey ConstantT Identity)

deriving instance Read (PrimaryKey ConstantT Identity)

deriving instance Show (PrimaryKey ConstantT Identity)

data Schema f = Schema
  { operations :: f (TableEntity OperationT)
  , constants :: f (TableEntity ConstantT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema
