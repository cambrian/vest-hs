module TezosInjector.Db
  ( module TezosInjector.Db
  ) where

import Postgres
import qualified Tezos
import Vest hiding (from, hash, to)

data OperationT f = Operation
  { signed_bytes :: C f Tezos.SignedOperation
  , object :: C f Tezos.OperationObject
  , hash :: C f (Maybe Tezos.OperationHash)
  , created_at :: C f Time
  , updated_at :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationSignedBytes (C f
                                                       Tezos.SignedOperation)
                                 deriving (Generic, Beamable)
  primaryKey Operation {signed_bytes} = OperationSignedBytes signed_bytes

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data PayoutT f = Payout
  { id :: C f UUID
  , signed_bytes :: PrimaryKey OperationT f
  , created_at :: C f Time
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

data VariableT f = Variable
  { key :: C f Text
  , value :: C f Word64 -- Change to Text and read/show if we ever get more variables.
  , created_at :: C f Time
  , updated_at :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Variable = VariableT Identity

deriving instance Eq Variable

deriving instance Read Variable

deriving instance Show Variable

instance Table VariableT where
  data PrimaryKey VariableT f = VariableKey (C f Text)
                                deriving (Generic, Beamable)
  primaryKey Variable {key} = VariableKey key

deriving instance Eq (PrimaryKey VariableT Identity)

deriving instance Read (PrimaryKey VariableT Identity)

deriving instance Show (PrimaryKey VariableT Identity)

data Schema f = Schema
  { operations :: f (TableEntity OperationT)
  , payouts :: f (TableEntity PayoutT)
  , variables :: f (TableEntity VariableT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema

vestCounterKey :: Text
vestCounterKey = "vestCounter"

selectPendingOperations :: Pg [(Tezos.SignedOperation, Tezos.OperationObject)]
selectPendingOperations =
  runSelectReturningList
    (select $
     fmap
       (\Operation {signed_bytes, object} -> (signed_bytes, object))
       (filter_ (\Operation {hash} -> hash ==. val_ Nothing) $
        all_ (operations schema)))

tryInsertVestCounter :: Time -> Word64 -> Pg ()
tryInsertVestCounter createdAt vestCounterValue =
  runInsert $
  insert
    (variables schema)
    (insertValues [Variable vestCounterKey vestCounterValue createdAt Nothing]) $
  onConflict anyConflict onConflictDoNothing

-- Runs an action with the current counter value and returns an (update time, new counter value)
-- tuple to persist. IMPORTANT: Do not modify the counter in the action.
withVestCounterTx :: (Word64 -> Pg (Time, Word64)) -> Pg Bool
withVestCounterTx action = do
  variableMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\Variable {key} -> key ==. val_ vestCounterKey) $
    all_ (variables schema)
  case variableMaybe of
    Nothing -> return False
    Just variable@Variable {value} -> do
      (updatedAt, newCounterValue) <- action value
      runUpdate $
        save
          (variables schema)
          (variable {value = newCounterValue, updated_at = Just updatedAt})
      return True

insertOperationTx ::
     Time -> Tezos.SignedOperation -> Tezos.OperationObject -> [UUID] -> Pg ()
insertOperationTx createdAt opSignedBytes object payoutIds = do
  runInsert $
    insert
      (operations schema)
      (insertValues [Operation opSignedBytes object Nothing createdAt Nothing])
      onConflictDefault
  runInsert $
    insert
      (payouts schema)
      (insertValues $
       fmap
         (\id -> Payout id (OperationSignedBytes opSignedBytes) createdAt)
         payoutIds)
      onConflictDefault

confirmOperationTx ::
     Time -> Tezos.SignedOperation -> Tezos.OperationHash -> Pg Bool
confirmOperationTx updatedAt opSignedBytes opHash = do
  operationMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\Operation {signed_bytes} -> signed_bytes ==. val_ opSignedBytes) $
    all_ (operations schema)
  case operationMaybe of
    Nothing -> return False
    Just operation -> do
      runUpdate $
        save
          (operations schema)
          (operation {hash = Just opHash, updated_at = Just updatedAt})
      return True
