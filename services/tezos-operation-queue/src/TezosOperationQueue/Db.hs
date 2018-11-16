module TezosOperationQueue.Db
  ( module TezosOperationQueue.Db
  ) where

import Postgres
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import Vest hiding (from, hash, to)

data BlockT f = Block
  { blockNumber :: C f Word64
  , blockTime :: C f Time
  , blockCreatedAt :: C f Time
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockNumber (C f Word64)
                             deriving (Generic, Beamable)
  primaryKey Block {blockNumber} = BlockNumber blockNumber

deriving instance Eq (PrimaryKey BlockT (Nullable Identity))

deriving instance Read (PrimaryKey BlockT (Nullable Identity))

deriving instance Show (PrimaryKey BlockT (Nullable Identity))

data OperationT f = Operation
  { operationContents :: C f Tezos.SignedOperationContents
  , operationBlockNumber :: PrimaryKey BlockT (Nullable f)
  , operationCreatedAt :: C f Time
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationContents (C f
                                                    Tezos.SignedOperationContents)
                                 deriving (Generic, Beamable)
  primaryKey Operation {operationContents} = OperationContents operationContents

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data Schema f = Schema
  { blocks :: f (TableEntity BlockT)
  , operations :: f (TableEntity OperationT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema

selectNextBlockNumber :: Pg Word64
selectNextBlockNumber = do
  m <-
    runSelectReturningOne $
    select $ aggregate_ max_ $ blockNumber <$> all_ (blocks schema)
  return $
    case m of
      Just (Just num) -> num + 1
      _ -> fromIntegral Tezos.firstReadableBlockNumber
