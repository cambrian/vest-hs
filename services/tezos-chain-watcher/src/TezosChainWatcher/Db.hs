{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Eventually: Remove redundancy in this module.
module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Postgres
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import Vest hiding (from, hash, state, to)

data BlockT f = Block
  { number :: C f Word64
  , hash :: C f Tezos.BlockHash
  , predecessor :: C f Tezos.BlockHash
  , cycle_number :: C f Word64
  , time :: C f Time
  , is_provisional :: C f Bool
  , is_deleted :: C f Bool
  , created_at :: C f Time
  , updated_at :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockHash (C f Tezos.BlockHash)
                             deriving (Generic, Beamable)
  primaryKey Block {hash} = BlockHash hash

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

data OperationT f = Operation
  { hash :: C f Tezos.OperationHash
  , block_hash :: PrimaryKey BlockT f
  , created_at :: C f Time
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
  { hash :: PrimaryKey OperationT f
  , originator :: C f Tezos.ImplicitAddress
  , originated :: C f Tezos.OriginatedAddress
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Origination = OriginationT Identity

deriving instance Eq Origination

deriving instance Read Origination

deriving instance Show Origination

instance Table OriginationT where
  data PrimaryKey OriginationT f = OriginationHash (PrimaryKey
                                                    OperationT
                                                    f)
                                                 (C f Tezos.ImplicitAddress)
                                                 (C f Tezos.OriginatedAddress)
                                   deriving (Generic, Beamable)
  primaryKey Origination {hash, originator, originated} =
    OriginationHash hash originator originated

deriving instance Eq (PrimaryKey OriginationT Identity)

deriving instance Read (PrimaryKey OriginationT Identity)

deriving instance Show (PrimaryKey OriginationT Identity)

data TransactionT f = Transaction
  { id :: C f UUID
  , hash :: PrimaryKey OperationT f
  , from :: C f Tezos.Address
  , to :: C f Tezos.Address
  , fee :: C f (FixedQty XTZ)
  , size :: C f (FixedQty XTZ)
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Transaction = TransactionT Identity

deriving instance Eq Transaction

deriving instance Read Transaction

deriving instance Show Transaction

instance Table TransactionT where
  data PrimaryKey TransactionT f = TransactionId (C f UUID)
                                   deriving (Generic, Beamable)
  primaryKey Transaction {id} = TransactionId id

deriving instance Eq (PrimaryKey TransactionT Identity)

deriving instance Read (PrimaryKey TransactionT Identity)

deriving instance Show (PrimaryKey TransactionT Identity)

data Schema f = Schema
  { blocks :: f (TableEntity BlockT)
  , operations :: f (TableEntity OperationT)
  , originations :: f (TableEntity OriginationT)
  , transactions :: f (TableEntity TransactionT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema

data BlockState
  = NotDeleted -- Just not deleted.
  | Provisional -- Provisional and not deleted.
  | Finalized -- Not provisional and not deleted.
  deriving (Eq)

-- This type signature is gnarly.
isProvisional Block {is_provisional, is_deleted} =
  (is_provisional ==. val_ True) &&. (is_deleted ==. val_ False)

-- This type signature is gnarly.
filterBlocksByState NotDeleted =
  filter_ (\Block {is_deleted} -> is_deleted ==. val_ False)
filterBlocksByState Provisional = filter_ isProvisional
filterBlocksByState Finalized =
  filter_
    (\Block {is_provisional, is_deleted} ->
       (is_provisional ==. val_ False) &&. (is_deleted ==. val_ False))

selectMaxBlockNumber :: BlockState -> Pg Word64
selectMaxBlockNumber state = do
  m <-
    runSelectReturningOne $
    select $
    aggregate_ max_ $
    number <$> filterBlocksByState state (all_ (blocks schema))
  return $
    case m of
      Just (Just num) -> num
      _ -> fromIntegral Tezos.firstReadableBlockNumber - 1

toTezosOrigination :: Origination -> Tezos.Origination
toTezosOrigination Origination { hash = OperationHash opHash
                               , originator
                               , originated
                               } =
  Tezos.Origination {hash = opHash, originator, originated}

toTezosTransaction :: Transaction -> Tezos.Transaction
toTezosTransaction Transaction { hash = OperationHash opHash
                               , from
                               , to
                               , fee
                               , size
                               } =
  Tezos.Transaction {hash = opHash, from, to, fee, size}

selectTezosOperationsByBlockHash :: Tezos.BlockHash -> Pg [Tezos.OperationHash]
selectTezosOperationsByBlockHash blockHash =
  map (\Operation {hash} -> hash) <$>
  runSelectReturningList
    (select $
     filter_ (\op -> block_hash op ==. val_ (BlockHash blockHash)) $
     all_ (operations schema))

selectTezosOriginationsByOpHash ::
     [Tezos.OperationHash] -> Pg [Tezos.Origination]
selectTezosOriginationsByOpHash opHashes =
  map toTezosOrigination <$>
  runSelectReturningList
    (select $
     filter_
       (\Origination {hash = OperationHash hash} -> hash `in_` map val_ opHashes) $
     all_ (originations schema))

selectTezosTransactionsByOpHash ::
     [Tezos.OperationHash] -> Pg [Tezos.Transaction]
selectTezosTransactionsByOpHash opHashes =
  map toTezosTransaction <$>
  runSelectReturningList
    (select $
     filter_
       (\Transaction {hash = OperationHash hash} -> hash `in_` map val_ opHashes) $
     all_ (transactions schema))

toBlockEvent :: Block -> Pg Tezos.BlockEvent
toBlockEvent (Block number hash predecessor cycleNumber time _ _ _ _) = do
  operations <- selectTezosOperationsByBlockHash hash
  originations <- selectTezosOriginationsByOpHash operations
  transactions <- selectTezosTransactionsByOpHash operations
  return $
    Tezos.BlockEvent
      { number
      , hash
      , predecessor
      , cycleNumber
      , time
      , operations
      , originations
      , transactions
      }

selectFinalizedBlockEventByNumber :: Word64 -> Pg (Maybe Tezos.BlockEvent)
selectFinalizedBlockEventByNumber blockNumber = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> number block ==. val_ blockNumber) $
    filterBlocksByState Finalized (all_ (blocks schema))
  case blockMaybe of
    Nothing -> return Nothing
    Just block -> Just <$> toBlockEvent block

selectBlockInfoForOpHash ::
     Tezos.OperationHash -> Pg (Maybe (Tezos.BlockHash, Word64))
selectBlockInfoForOpHash opHash =
  runSelectReturningOne $
  select $ do
    op <-
      filter_ (\Operation {hash} -> hash ==. val_ opHash) $
      all_ (operations schema)
    Block {hash, number} <-
      filterBlocksByState NotDeleted $ all_ (blocks schema)
    guard_ (block_hash op ==. BlockHash hash)
    pure (hash, number)

insertTezosOpHashes :: Tezos.BlockHash -> Time -> [Tezos.OperationHash] -> Pg ()
insertTezosOpHashes blockHash createdAt tezosOpHashes =
  runInsert $
  insert
    (operations schema)
    (insertValues $
     map (\hash -> Operation hash (BlockHash blockHash) createdAt) tezosOpHashes)
    onConflictDefault

insertTezosOriginations :: Time -> [Tezos.Origination] -> Pg ()
insertTezosOriginations createdAt tezosOriginations =
  runInsert $
  insert
    (originations schema)
    (insertValues $
     map
       (\Tezos.Origination {hash, originator, originated} ->
          Origination (OperationHash hash) originator originated createdAt)
       tezosOriginations)
    onConflictDefault

insertTezosTransactions :: Time -> [Tezos.Transaction] -> Pg ()
insertTezosTransactions createdAt tezosTransactions = do
  dbTransactions <-
    mapM
      (\Tezos.Transaction {hash, from, to, fee, size} -> do
         id <- liftIO nextUUID
         return $ Transaction id (OperationHash hash) from to fee size createdAt)
      tezosTransactions
  runInsert $
    insert (transactions schema) (insertValues dbTransactions) onConflictDefault

insertProvisionalBlockEventTx :: Time -> Tezos.BlockEvent -> Pg ()
insertProvisionalBlockEventTx createdAt blockEvent = do
  let Tezos.BlockEvent { number
                       , hash
                       , predecessor
                       , cycleNumber
                       , time
                       , operations
                       , originations
                       , transactions
                       } = blockEvent
  block <- runSelectReturningOne (lookup_ (blocks schema) (BlockHash hash))
  case block of
    Just block -> do
      let updatedAt = createdAt
      runUpdate $
        save
          (blocks schema)
          (block {is_deleted = False, updated_at = Just updatedAt})
    -- ^ If the provisional block already exists, we have switched back from another chain fork and
    -- need to un-delete this particular block.
    Nothing -> do
      runInsert $
        insert
          (blocks schema)
          (insertValues
             [ Block
                 number
                 hash
                 predecessor
                 cycleNumber
                 time
                 True
                 False
                 createdAt
                 Nothing
             ])
          onConflictDefault
      insertTezosOpHashes hash createdAt operations
      insertTezosOriginations createdAt originations
      insertTezosTransactions createdAt transactions

-- Finalizes blocks less than or equal to the max number to finalize. Queries the given blocks to be
-- returned as block events (prior to actually performing the update).
finalizeProvisionalBlockEventsTx :: Time -> Word64 -> Pg [Tezos.BlockEvent]
finalizeProvisionalBlockEventsTx updatedAt maxBlockNumberToFinalize = do
  blocksAffected <-
    runSelectReturningList $
    select $
    filter_ (\Block {number} -> number <=. val_ maxBlockNumberToFinalize) $
    filterBlocksByState Provisional $ all_ (blocks schema)
  runUpdate $
    update
      (blocks schema)
      (\block ->
         [ is_provisional block <-. val_ False
         , updated_at block <-. val_ (Just updatedAt)
         ])
      (\block@Block {number} ->
         number <=. val_ maxBlockNumberToFinalize &&. isProvisional block)
  blockEvents <- mapM toBlockEvent blocksAffected
  return $ sortOn (\Tezos.BlockEvent {number} -> number) blockEvents

-- | Soft delete by setting a boolean.
deleteProvisionalBlockEventsTx :: Time -> [Tezos.BlockHash] -> Pg ()
deleteProvisionalBlockEventsTx deletedAt blockHashes =
  runUpdate $
  update
    (blocks schema)
    (\block ->
       [ is_deleted block <-. val_ True
       , updated_at block <-. val_ (Just deletedAt)
       ])
    (\block@Block {hash} ->
       (hash `in_` map val_ blockHashes) &&. isProvisional block)
