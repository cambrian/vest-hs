module TezosHotWallet.Db
  ( module TezosHotWallet.Db
  ) where

import Postgres
import qualified Tezos
import Vest

data BlockT f = Block
  { number :: C f Word64
  , createdAt :: C f Time
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

data PayoutT f = Payout
  { id :: C f UUID
  , size :: C f (FixedQty XTZ)
  , recipient :: C f Tezos.Address
  , opHash :: C (Nullable f) Tezos.OperationHash
  , createdAt :: C f Time
  } deriving (Generic, Beamable)

type Payout = PayoutT Identity

deriving instance Eq Payout

deriving instance Read Payout

deriving instance Show Payout

instance Table PayoutT where
  data PrimaryKey PayoutT f = PayoutId (C f UUID)
                              deriving (Generic, Beamable)
  primaryKey Payout {id} = PayoutId id

deriving instance Eq (PrimaryKey PayoutT Identity)

deriving instance Read (PrimaryKey PayoutT Identity)

deriving instance Show (PrimaryKey PayoutT Identity)

data Schema f = Schema
  { blocks :: f (TableEntity BlockT)
  , payouts :: f (TableEntity PayoutT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema
