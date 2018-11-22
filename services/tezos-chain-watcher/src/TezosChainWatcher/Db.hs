{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- TODO: Remove redundancy in this module.
module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Postgres
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import Vest hiding (from, hash, state, to)

data BlockT f = Block
  { blockNumber :: C f Word64
  , blockProvisionalId :: C f Int
  , blockHash :: C f Tezos.BlockHash
  , blockPredecessor :: C f Tezos.BlockHash
  , blockCycleNumber :: C f Word64
  , blockFee :: C f (FixedQty XTZ)
  , blockIsProvisional :: C f Bool
  , blockTime :: C f Time
  , blockCreatedAt :: C f Time
  , blockUpdatedAt :: C f (Maybe Time)
  , blockDeletedAt :: C f (Maybe Time)
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockHash (C f Tezos.BlockHash)
                             deriving (Generic, Beamable)
  primaryKey Block {blockHash} = BlockHash blockHash

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

data OperationT f = Operation
  { operationHash :: C f Tezos.OperationHash
  , operationBlockHash :: PrimaryKey BlockT f
  , operationCreatedAt :: C f Time
  } deriving (Generic, Beamable)

type Operation = OperationT Identity

deriving instance Eq Operation

deriving instance Read Operation

deriving instance Show Operation

instance Table OperationT where
  data PrimaryKey OperationT f = OperationHash (C f
                                                Tezos.OperationHash)
                                 deriving (Generic, Beamable)
  primaryKey Operation {operationHash} = OperationHash operationHash

deriving instance Eq (PrimaryKey OperationT Identity)

deriving instance Read (PrimaryKey OperationT Identity)

deriving instance Show (PrimaryKey OperationT Identity)

data OriginationT f = Origination
  { originationHash :: PrimaryKey OperationT f
  , originationOriginator :: C f Tezos.ImplicitAddress
  , originationOriginated :: C f Tezos.OriginatedAddress
  , originationCreatedAt :: C f Time
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
  primaryKey Origination { originationHash
                         , originationOriginator
                         , originationOriginated
                         } =
    OriginationHash originationHash originationOriginator originationOriginated

deriving instance Eq (PrimaryKey OriginationT Identity)

deriving instance Read (PrimaryKey OriginationT Identity)

deriving instance Show (PrimaryKey OriginationT Identity)

data TransactionT f = Transaction
  { transactionHash :: PrimaryKey OperationT f
  , transactionFrom :: C f Tezos.Address
  , transactionTo :: C f Tezos.Address
  , transactionFee :: C f (FixedQty XTZ)
  , transactionSize :: C f (FixedQty XTZ)
  , transactionCreatedAt :: C f Time
  } deriving (Generic, Beamable)

type Transaction = TransactionT Identity

deriving instance Eq Transaction

deriving instance Read Transaction

deriving instance Show Transaction

instance Table TransactionT where
  data PrimaryKey TransactionT f = TransactionHash (PrimaryKey
                                                    OperationT
                                                    f)
                                   deriving (Generic, Beamable)
  primaryKey Transaction {transactionHash} = TransactionHash transactionHash

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
  = Provisional
  | Final
  deriving (Eq)

-- This type signature is gnarly.
filterBlocksByState state =
  if state == Provisional
    then filter_ (\Block {blockDeletedAt} -> blockDeletedAt ==. val_ Nothing)
    else filter_
           (\Block {blockIsProvisional, blockDeletedAt} ->
              (blockIsProvisional ==. val_ False) &&.
              (blockDeletedAt ==. val_ Nothing))

selectMaxBlockNumber :: BlockState -> Pg Word64
selectMaxBlockNumber state = do
  m <-
    runSelectReturningOne $
    select $
    aggregate_ max_ $
    blockNumber <$> filterBlocksByState state (all_ (blocks schema))
  return $
    case m of
      Just (Just num) -> num
      _ -> fromIntegral Tezos.firstReadableBlockNumber - 1

selectMaxBlockProvisionalId :: Pg Int
selectMaxBlockProvisionalId = do
  m <-
    runSelectReturningOne $
    select $ aggregate_ max_ $ blockProvisionalId <$> all_ (blocks schema)
  return $
    case m of
      Just (Just num) -> num
      _ -> -1

toTezosOrigination :: Origination -> Tezos.Origination
toTezosOrigination Origination { originationHash = OperationHash opHash
                               , originationOriginator = originator
                               , originationOriginated = originated
                               } =
  Tezos.Origination {hash = opHash, originator, originated}

toTezosTransaction :: Transaction -> Tezos.Transaction
toTezosTransaction Transaction { transactionHash = OperationHash opHash
                               , transactionFrom = from
                               , transactionTo = to
                               , transactionFee = fee
                               , transactionSize = size
                               } =
  Tezos.Transaction {hash = opHash, from, to, fee, size}

selectTezosOperationsByBlockHash :: Tezos.BlockHash -> Pg [Tezos.OperationHash]
selectTezosOperationsByBlockHash hash =
  fmap operationHash <$>
  runSelectReturningList
    (select $
     filter_ (\op -> operationBlockHash op ==. val_ (BlockHash hash)) $
     all_ (operations schema))

selectTezosOriginationsByOpHash :: Tezos.OperationHash -> Pg [Tezos.Origination]
selectTezosOriginationsByOpHash hash =
  fmap toTezosOrigination <$>
  runSelectReturningList
    (select $
     filter_ (\og -> originationHash og ==. val_ (OperationHash hash)) $
     all_ (originations schema))

selectTezosTransactionsByOpHash :: Tezos.OperationHash -> Pg [Tezos.Transaction]
selectTezosTransactionsByOpHash hash =
  fmap toTezosTransaction <$>
  runSelectReturningList
    (select $
     filter_ (\og -> transactionHash og ==. val_ (OperationHash hash)) $
     all_ (transactions schema))

toBlockEvent :: Block -> Pg Tezos.BlockEvent
toBlockEvent (Block number _ hash predecessor cycleNumber fee _ time _ _ _) = do
  operations <- selectTezosOperationsByBlockHash hash
  originations <- concatMapM selectTezosOriginationsByOpHash operations
  transactions <- concatMapM selectTezosTransactionsByOpHash operations
  return $
    Tezos.BlockEvent
      { number
      , hash
      , predecessor
      , cycleNumber
      , fee
      , time
      , operations
      , originations
      , transactions
      }

selectBlockEventByProvisionalId :: Int -> Pg (Maybe Tezos.BlockEvent)
selectBlockEventByProvisionalId provisionalId = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> blockProvisionalId block ==. val_ provisionalId) $
    all_ (blocks schema)
  case blockMaybe of
    Nothing -> return Nothing
    Just block -> Just <$> toBlockEvent block

-- Non-provisional blocks only.
selectFinalBlockEventByNumber :: Word64 -> Pg (Maybe Tezos.BlockEvent)
selectFinalBlockEventByNumber queryNumber = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> blockNumber block ==. val_ queryNumber) $
    filterBlocksByState Final (all_ (blocks schema))
  case blockMaybe of
    Nothing -> return Nothing
    Just block -> Just <$> toBlockEvent block

insertTezosOpHashes :: Tezos.BlockHash -> Time -> [Tezos.OperationHash] -> Pg ()
insertTezosOpHashes blockHash createdAt tezosOpHashes =
  runInsert $
  insert
    (operations schema)
    (insertValues $
     fmap
       (\hash -> Operation hash (BlockHash blockHash) createdAt)
       tezosOpHashes) $
  onConflict anyConflict onConflictDoNothing

insertTezosOriginations :: Time -> [Tezos.Origination] -> Pg ()
insertTezosOriginations createdAt tezosOriginations =
  runInsert $
  insert
    (originations schema)
    (insertValues $
     fmap
       (\Tezos.Origination {hash, originator, originated} ->
          Origination (OperationHash hash) originator originated createdAt)
       tezosOriginations) $
  onConflict anyConflict onConflictDoNothing

insertTezosTransactions :: Time -> [Tezos.Transaction] -> Pg ()
insertTezosTransactions createdAt tezosTransactions =
  runInsert $
  insert
    (transactions schema)
    (insertValues $
     fmap
       (\Tezos.Transaction {hash, from, to, fee, size} ->
          Transaction (OperationHash hash) from to fee size createdAt)
       tezosTransactions) $
  onConflict anyConflict onConflictDoNothing

insertProvisionalBlockEvent :: Time -> Int -> Tezos.BlockEvent -> Pg ()
insertProvisionalBlockEvent createdAt provisionalId blockEvent = do
  let Tezos.BlockEvent { number
                       , hash
                       , predecessor
                       , cycleNumber
                       , fee
                       , time
                       , operations
                       , originations
                       , transactions
                       } = blockEvent
  runInsert $
    insert
      (blocks schema)
      (insertValues
         [ Block
             number
             provisionalId
             hash
             predecessor
             cycleNumber
             fee
             True
             time
             createdAt
             Nothing
             Nothing
         ]) $
    onConflict anyConflict onConflictDoNothing
  insertTezosOpHashes hash createdAt operations
  insertTezosOriginations createdAt originations
  insertTezosTransactions createdAt transactions

finalizeProvisionalBlockEvent :: Time -> Tezos.BlockHash -> Pg Bool
finalizeProvisionalBlockEvent updatedAt hash = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> blockHash block ==. val_ hash) $
    filterBlocksByState Provisional $ all_ (blocks schema)
  case blockMaybe of
    Nothing -> return False
    Just block -> do
      runUpdate $
        save
          (blocks schema)
          (block {blockIsProvisional = False, blockUpdatedAt = Just updatedAt})
      return True

-- | Soft delete by setting deletedAt.
deleteProvisionalBlockEventsByHash :: Time -> [Tezos.BlockHash] -> Pg ()
deleteProvisionalBlockEventsByHash deletedAt hashes = do
  blocksToDelete <-
    runSelectReturningList $
    select $
    filter_ (\block -> blockHash block `in_` fmap val_ hashes) $
    filterBlocksByState Provisional $ all_ (blocks schema)
  mapM_
    (\block ->
       runUpdate $
       save (blocks schema) (block {blockDeletedAt = Just deletedAt}))
    blocksToDelete

-- | Soft delete by setting deletedAt.
deleteOldProvisionalBlockEvents :: Time -> Word64 -> Pg ()
deleteOldProvisionalBlockEvents deletedAt maxBlockNumberToDelete = do
  blocksToDelete <-
    runSelectReturningList $
    select $
    filter_ (\block -> blockNumber block <=. val_ maxBlockNumberToDelete) $
    filterBlocksByState Provisional $ all_ (blocks schema)
  mapM_
    (\block ->
       runUpdate $
       save (blocks schema) (block {blockDeletedAt = Just deletedAt}))
    blocksToDelete
