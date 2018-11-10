module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Db
import qualified GHC.Base
import qualified Tezos
import Vest hiding (from, hash, to)

data BlockT f = Block
  { number :: C f Word64
  , hash :: C f Tezos.BlockHash
  , cycleNumber :: C f Word64
  , fee :: C f (FixedQty XTZ)
  , time :: C f Time
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

data OperationKind
  = OriginationKind
  | TransactionKind
  | OtherKind
  deriving (Eq, Read, Show)

instance HasSqlValueSyntax be GHC.Base.String =>
         HasSqlValueSyntax be OperationKind where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField OperationKind where
  fromField f bs = do
    valMaybe <- deserialize @'Haskell <$> fromField f bs
    case valMaybe of
      Nothing -> returnError ConversionFailed f "could not parse OperationKind"
      Just val -> pure val

instance FromBackendRow Postgres OperationKind

data OperationT f = Operation
  { hash :: C f Tezos.OperationHash
  , blockNumber :: PrimaryKey BlockT f
  , kind :: C f OperationKind
  , createdAt :: C f Time
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
  , createdAt :: C f Time
  } deriving (Generic, Beamable)

type Origination = OriginationT Identity

deriving instance Eq Origination

deriving instance Read Origination

deriving instance Show Origination

instance Table OriginationT where
  data PrimaryKey OriginationT f = OriginationHash (PrimaryKey
                                                    OperationT
                                                    f)
                                   deriving (Generic, Beamable)
  primaryKey Origination {hash} = OriginationHash hash

deriving instance Eq (PrimaryKey OriginationT Identity)

deriving instance Read (PrimaryKey OriginationT Identity)

deriving instance Show (PrimaryKey OriginationT Identity)

data TransactionT f = Transaction
  { hash :: PrimaryKey OperationT f
  , from :: C f Tezos.Address
  , to :: C f Tezos.Address
  , fee :: C f (FixedQty XTZ)
  , size :: C f (FixedQty XTZ)
  , createdAt :: C f Time
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
  primaryKey Transaction {hash} = TransactionHash hash

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

schema :: DatabaseSettings Postgres Schema
schema = defaultDbSettings

selectNextBlockNumber :: Pg Word64
selectNextBlockNumber = do
  m <-
    runSelectReturningOne $
    select $ aggregate_ max_ $ number <$> all_ (blocks schema)
  return $
    case m of
      Just (Just num) -> num + 1
      _ -> fromIntegral Tezos.firstBlockNumber

-- TODO: Reduce duplication.
selectNextCycleNumber :: Pg Word64
selectNextCycleNumber = do
  m <-
    runSelectReturningOne $
    select $ aggregate_ max_ $ cycleNumber <$> all_ (blocks schema)
  return $
    case m of
      Just (Just num) -> num + 1
      _ -> 0

selectBlockInfoForOpHash ::
     Tezos.OperationHash -> Pg (Maybe (Word64, Tezos.BlockHash))
selectBlockInfoForOpHash opHash =
  runSelectReturningOne $
  select $ do
    op <-
      filter_ (\Operation {hash} -> hash ==. val_ opHash) $
      all_ (operations schema)
    block <- all_ (blocks schema)
    let BlockNumber opBlockNumber = blockNumber op
    guard_ (opBlockNumber ==. number block)
    let Block {number, hash} = block
    pure (number, hash)

selectOriginatedForImplicit ::
     Tezos.ImplicitAddress -> Pg [Tezos.OriginatedAddress]
selectOriginatedForImplicit implicitAddress = do
  originations <-
    runSelectReturningList $
    select $
    filter_ (\Origination {originator} -> originator ==. val_ implicitAddress) $
    all_ (originations schema)
  return $ originated <$> originations

originationToOp :: Origination -> Tezos.Operation
originationToOp Origination { hash = OperationHash opHash
                            , originator
                            , originated
                            } =
  Tezos.OriginationOp $ Tezos.Origination opHash originator originated

transactionToOp :: Transaction -> Tezos.Operation
transactionToOp Transaction {hash = OperationHash opHash, from, to, fee, size} =
  Tezos.TransactionOp $ Tezos.Transaction opHash from to fee size

toBlockEventOperation :: Operation -> Pg (Maybe Tezos.Operation)
toBlockEventOperation Operation {hash, kind} =
  case kind of
    OriginationKind -> do
      originationMaybe <-
        runSelectReturningOne $
        select $
        filter_
          (\Origination {hash = OperationHash opHash} -> opHash ==. val_ hash) $
        all_ (originations schema)
      return $ originationToOp <$> originationMaybe
    TransactionKind -> do
      transactionMaybe <-
        runSelectReturningOne $
        select $
        filter_
          (\Transaction {hash = OperationHash opHash} -> opHash ==. val_ hash) $
        all_ (transactions schema)
      return $ transactionToOp <$> transactionMaybe
    OtherKind -> return $ Just $ Tezos.Other hash

-- TODO: Refactor using guard syntax.
selectBlockEventByNumber ::
     Word64 -> Pg (Either InvalidStateException (Maybe Tezos.BlockEvent))
selectBlockEventByNumber queryNumber = do
  blockMaybe <-
    runSelectReturningOne $
    select $
    filter_ (\block -> number block ==. val_ queryNumber) $ all_ (blocks schema)
  case blockMaybe of
    Nothing -> return $ Right Nothing
    Just Block {number, hash, cycleNumber, fee, time} -> do
      blockOps <-
        runSelectReturningList $
        select $
        filter_ (\op -> blockNumber op ==. BlockNumber (val_ queryNumber)) $
        all_ (operations schema)
      operations <- catMaybes <$> mapM toBlockEventOperation blockOps
      if length blockOps /= length operations
        then return $ Left InvalidStateException
        else return $
             Right . Just $
             Tezos.BlockEvent {number, hash, cycleNumber, fee, time, operations}

selectCycleEventByNumber :: Word64 -> Pg (Maybe Tezos.CycleEvent)
selectCycleEventByNumber queryNumber = do
  firstBlockInNextCycle <-
    runSelectReturningOne $
    select $
    filter_ (\block -> cycleNumber block ==. val_ (queryNumber + 1)) $
    all_ (blocks schema)
  return $
    (\Block {time} -> Tezos.CycleEvent queryNumber time) <$>
    firstBlockInNextCycle

-- TODO: Reduce verbose duplication. Also, batching?
insertOp :: Word64 -> Time -> Tezos.Operation -> Pg ()
insertOp blockNumber createdAt op =
  case op of
    Tezos.OriginationOp Tezos.Origination {hash, originator, originated} -> do
      runInsert $
        insert
          (operations schema)
          (insertValues
             [ Operation
                 hash
                 (BlockNumber blockNumber)
                 OriginationKind
                 createdAt
             ])
          onConflictDefault
      runInsert $
        insert
          (originations schema)
          (insertValues
             [Origination (OperationHash hash) originator originated createdAt])
          onConflictDefault
    Tezos.TransactionOp Tezos.Transaction {hash, from, to, fee, size} -> do
      runInsert $
        insert
          (operations schema)
          (insertValues
             [ Operation
                 hash
                 (BlockNumber blockNumber)
                 TransactionKind
                 createdAt
             ])
          onConflictDefault
      runInsert $
        insert
          (transactions schema)
          (insertValues
             [Transaction (OperationHash hash) from to fee size createdAt])
          onConflictDefault
    Tezos.Other hash ->
      runInsert $
      insert
        (operations schema)
        (insertValues
           [Operation hash (BlockNumber blockNumber) OtherKind createdAt])
        onConflictDefault

insertBlockEvent :: Time -> Tezos.BlockEvent -> Pg ()
insertBlockEvent createdAt blockEvent = do
  let Tezos.BlockEvent {number, hash, cycleNumber, fee, time, operations} =
        blockEvent
  runInsert $
    insert
      (blocks schema)
      (insertValues [Block number hash cycleNumber fee time createdAt])
      onConflictDefault
  mapM_ (insertOp number createdAt) operations
