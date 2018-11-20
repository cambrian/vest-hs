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

instance IndexableTable BlockT where
  indexColumn = number

data PayoutT f = Payout
  { id :: C f UUID
  , to :: C f Tezos.Address
  , size :: C f (FixedQty XTZ)
  , hash :: C (Nullable f) Tezos.OperationHash
  , status :: C f Text -- TODO: use OperationStatus for this. Hard bc enums are not supported by beam
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

data PaymentT f = Payment
  { idx :: C f Word64
  , hash :: C f Tezos.OperationHash
  , from :: C f Tezos.Address
  , size :: C f (FixedQty XTZ)
  , time :: C f Time
  , createdAt :: C f Time
  } deriving (Generic, Beamable)

type Payment = PaymentT Identity

deriving instance Eq Payment

deriving instance Read Payment

deriving instance Show Payment

instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentIdx (C f Word64)
                               deriving (Generic, Beamable)
  primaryKey Payment {idx} = PaymentIdx idx

deriving instance Eq (PrimaryKey PaymentT Identity)

deriving instance Read (PrimaryKey PaymentT Identity)

deriving instance Show (PrimaryKey PaymentT Identity)

instance IndexableTable PaymentT where
  indexColumn = idx

data Schema f = Schema
  { blocks :: f (TableEntity BlockT)
  , payouts :: f (TableEntity PayoutT)
  , payments :: f (TableEntity PaymentT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema

selectPayoutsByStatus :: Text -> Pg [Payout]
selectPayoutsByStatus status_ =
  runSelectReturningList $
  select $
  filter_ (\Payout {status} -> status ==. val_ status_) $ all_ $ payouts schema
