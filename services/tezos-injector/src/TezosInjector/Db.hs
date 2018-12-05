module TezosInjector.Db
  ( module TezosInjector.Db
  ) where

import GHC.Exts (groupWith)
import Postgres
import qualified Tezos
import Vest hiding (from, hash, to)

data OperationT f = Operation
  { id :: C f Word64 -- Used for ordering.
  , counter_address :: C f Tezos.Address
  , signed_bytes :: C f Tezos.SignedOperation
  , object :: C f Tezos.OperationObject
  , hash :: C f (Maybe Tezos.OperationHash)
  , is_internal :: C f Bool
  , is_rejected :: C f Bool
  , created_at :: C f Time
  , updated_at :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationId (C f Word64)
                                 deriving (Generic, Beamable)
  primaryKey Operation {id} = OperationId id

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data PayoutT f = Payout
  { id :: C f UUID
  , operation_id :: PrimaryKey OperationT f
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

selectNextOperationId :: Pg Word64
selectNextOperationId = do
  m <-
    runSelectReturningOne $
    select $
    aggregate_ max_ $ map (\Operation {id} -> id) $ all_ (operations schema)
  return $
    case m of
      Just (Just num) -> num + 1
      _ -> 0

selectNextOperations :: Pg [Operation]
selectNextOperations = do
  pendingOperations <-
    runSelectReturningList
      (select $
       filter_
         (\Operation {hash, is_rejected} ->
            (hash ==. val_ Nothing) &&. (is_rejected ==. val_ False)) $
       all_ (operations schema))
  let pendingByAccount = groupWith (\Operation {id} -> id) pendingOperations
      nextOperations =
        map (minimumBy $ comparing (\Operation {id} -> id)) pendingByAccount
  return nextOperations

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
     Time
  -> Tezos.Address
  -> Tezos.SignedOperation
  -> Tezos.OperationObject
  -> Bool
  -> [UUID]
  -> Pg ()
insertOperationTx createdAt opAddress opSignedBytes opObject opIsInternal payoutIds = do
  nextOpId <- selectNextOperationId
  runInsert $
    insert
      (operations schema)
      (insertValues
         [ Operation
             nextOpId
             opAddress
             opSignedBytes
             opObject
             Nothing
             opIsInternal
             False
             createdAt
             Nothing
         ])
      onConflictDefault
  runInsert $
    insert
      (payouts schema)
      (insertValues $
       map (\id -> Payout id (OperationId nextOpId) createdAt) payoutIds)
      onConflictDefault

confirmOperationTx :: Time -> Word64 -> Tezos.OperationHash -> Pg Bool
confirmOperationTx updatedAt opId opHash = do
  operationMaybe <-
    runSelectReturningOne $
    select $
    filter_
      (\Operation {id, is_rejected} ->
         (id ==. val_ opId) &&. (is_rejected ==. val_ False)) $
    all_ (operations schema)
  case operationMaybe of
    Nothing -> return False
    Just operation -> do
      runUpdate $
        save
          (operations schema)
          (operation {hash = Just opHash, updated_at = Just updatedAt})
      return True

rejectOperationTx :: Time -> Word64 -> Pg Bool
rejectOperationTx updatedAt opId = do
  operationMaybe <-
    runSelectReturningOne $
    select $
    filter_
      (\Operation {id, is_rejected} ->
         (id ==. val_ opId) &&. (is_rejected ==. val_ False)) $
    all_ (operations schema)
  case operationMaybe of
    Nothing -> return False
    Just operation -> do
      runUpdate $
        save
          (operations schema)
          (operation {is_rejected = True, updated_at = Just updatedAt})
      return True
