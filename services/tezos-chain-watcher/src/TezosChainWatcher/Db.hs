module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Postgres
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import Vest hiding (from, hash, to)

data BlockT f = Block
  { blockNumber :: C f Word64
  , blockHash :: C f Tezos.BlockHash
  , blockCycleNumber :: C f Word64
  , blockFee :: C f (FixedQty XTZ)
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

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

data OperationT f = Operation
  { operationHash :: C f Tezos.OperationHash
  , operationBlockNumber :: PrimaryKey BlockT f
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

selectNextBlockNumber :: Pg Word64
selectNextBlockNumber = do
  m <-
    runSelectReturningOne $
    select $ aggregate_ max_ $ blockNumber <$> all_ (blocks schema)
  return $
    case m of
      Just (Just num) -> num + 1
      _ -> fromIntegral Tezos.firstReadableBlockNumber + 1

selectBlockInfoForOpHash ::
     Tezos.OperationHash -> Pg (Maybe (Word64, Tezos.BlockHash))
selectBlockInfoForOpHash hash =
  runSelectReturningOne $
  select $ do
    op <-
      filter_ (\Operation {operationHash} -> operationHash ==. val_ hash) $
      all_ (operations schema)
    block <- all_ (blocks schema)
    guard_ (operationBlockNumber op ==. BlockNumber (blockNumber block))
    let Block {blockNumber, blockHash} = block
    pure (blockNumber, blockHash)

selectOriginatedForImplicit ::
     Tezos.ImplicitAddress -> Pg [Tezos.OriginatedAddress]
selectOriginatedForImplicit implicitAddress =
  runSelectReturningList $
  select $ do
    origination <-
      filter_
        (\Origination {originationOriginator} ->
           originationOriginator ==. val_ implicitAddress) $
      all_ (originations schema)
    pure $ originationOriginated origination

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

selectTezosOperationsByBlockNumber :: Word64 -> Pg [Tezos.OperationHash]
selectTezosOperationsByBlockNumber number =
  fmap operationHash <$>
  runSelectReturningList
    (select $
     filter_ (\op -> operationBlockNumber op ==. val_ (BlockNumber number)) $
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

selectBlockEventByNumber :: Word64 -> Pg (Maybe Tezos.BlockEvent)
selectBlockEventByNumber queryNumber = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> blockNumber block ==. val_ queryNumber) $
    all_ (blocks schema)
  case blockMaybe of
    Nothing -> return Nothing
    Just (Block number hash cycleNumber fee time _) -> do
      operations <- selectTezosOperationsByBlockNumber number
      originations <- concatMapM selectTezosOriginationsByOpHash operations
      transactions <- concatMapM selectTezosTransactionsByOpHash operations
      return $
        Just $
        Tezos.BlockEvent
          { number
          , hash
          , cycleNumber
          , fee
          , time
          , operations
          , originations
          , transactions
          }

insertTezosOpHashes :: Word64 -> Time -> [Tezos.OperationHash] -> Pg ()
insertTezosOpHashes blockNumber createdAt tezosOpHashes =
  runInsert $
  insert
    (operations schema)
    (insertValues $
     fmap
       (\hash -> Operation hash (BlockNumber blockNumber) createdAt)
       tezosOpHashes)
    onConflictDefault

insertTezosOriginations :: Time -> [Tezos.Origination] -> Pg ()
insertTezosOriginations createdAt tezosOriginations =
  runInsert $
  insert
    (originations schema)
    (insertValues $
     fmap
       (\Tezos.Origination {hash, originator, originated} ->
          Origination (OperationHash hash) originator originated createdAt)
       tezosOriginations)
    onConflictDefault

insertTezosTransactions :: Time -> [Tezos.Transaction] -> Pg ()
insertTezosTransactions createdAt tezosTransactions =
  runInsert $
  insert
    (transactions schema)
    (insertValues $
     fmap
       (\Tezos.Transaction {hash, from, to, fee, size} ->
          Transaction (OperationHash hash) from to fee size createdAt)
       tezosTransactions)
    onConflictDefault

insertBlockEvent :: Time -> Tezos.BlockEvent -> Pg ()
insertBlockEvent createdAt blockEvent = do
  let Tezos.BlockEvent { number
                       , hash
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
      (insertValues [Block number hash cycleNumber fee time createdAt])
      onConflictDefault
  insertTezosOpHashes number createdAt operations
  insertTezosOriginations createdAt originations
  insertTezosTransactions createdAt transactions
