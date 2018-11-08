module TezosDelegationCore.Db
  ( module TezosDelegationCore.Db
  ) where

import Db
import qualified Tezos
import Vest

data CycleT f = Cycle
  { number :: C f Word64
  , timestamp :: C f Timestamp
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Cycle = CycleT Identity

deriving instance Eq Cycle

deriving instance Read Cycle

deriving instance Show Cycle

instance Table CycleT where
  data PrimaryKey CycleT f = CycleNumber{cycleNumber :: C f Word64}
                             deriving (Generic, Beamable)
  primaryKey Cycle {number} = CycleNumber number

deriving instance Eq (PrimaryKey CycleT Identity)

deriving instance Read (PrimaryKey CycleT Identity)

deriving instance Show (PrimaryKey CycleT Identity)

data DelegateT f = Delegate
  { address :: C f Tezos.ImplicitAddress
  , name :: C f Text
  , description :: C f Text
  , firstManagedCycle :: PrimaryKey CycleT f
  , priceTiers :: C f (PgJSON [(FixedQty XTZ, Rational)])
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Delegate = DelegateT Identity

deriving instance Eq Delegate

deriving instance Read Delegate

deriving instance Show Delegate

instance Table DelegateT where
  data PrimaryKey DelegateT f = DelegateAddress (C f
                                                 Tezos.ImplicitAddress)
                                deriving (Generic, Beamable)
  primaryKey Delegate {address} = DelegateAddress address

deriving instance Eq (PrimaryKey DelegateT Identity)

deriving instance Read (PrimaryKey DelegateT Identity)

deriving instance Show (PrimaryKey DelegateT Identity)

-- | Not accepting partial payments at the moment. Would be relatively simple to extend.
-- (replace @open with a @sizeFulfilled column)
data BillT f = Bill
  { id :: C f UUID
  , delegate :: PrimaryKey DelegateT f
  , size :: C f (FixedQty XTZ)
  , open :: C f Bool
  , late :: C f Bool
  , timestamp :: C f Timestamp
  , dueAt :: C f Timestamp
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Bill = BillT Identity

deriving instance Eq Bill

deriving instance Read Bill

deriving instance Show Bill

instance Table BillT where
  data PrimaryKey BillT f = BillId (C f UUID)
                            deriving (Generic, Beamable)
  primaryKey Bill {id} = BillId id

deriving instance Eq (PrimaryKey BillT Identity)

deriving instance Read (PrimaryKey BillT Identity)

deriving instance Show (PrimaryKey BillT Identity)

data RewardT f = Reward
  { delegate :: PrimaryKey DelegateT f
  , cycle :: PrimaryKey CycleT f
  , size :: C f (FixedQty XTZ)
  , stakingBalace :: C f (FixedQty XTZ)
  , delegatedBalance :: C f (FixedQty XTZ)
  , bill :: PrimaryKey BillT f
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Reward = RewardT Identity

deriving instance Eq Reward

deriving instance Read Reward

deriving instance Show Reward

instance Table RewardT where
  data PrimaryKey RewardT f = RewardDelegateAndCycle (PrimaryKey
                                                      DelegateT
                                                      f)
                                                   (PrimaryKey CycleT f)
                              deriving (Generic, Beamable)
  primaryKey Reward {delegate, cycle} = RewardDelegateAndCycle delegate cycle

deriving instance Eq (PrimaryKey RewardT Identity)

deriving instance Read (PrimaryKey RewardT Identity)

deriving instance Show (PrimaryKey RewardT Identity)

data DividendT f = Dividend
  { delegator :: C f Tezos.OriginatedAddress
  , delegate :: PrimaryKey DelegateT f
  , cycle :: PrimaryKey CycleT f
  , size :: C f (FixedQty XTZ)
  , bill :: PrimaryKey BillT f
  , payout :: PrimaryKey PayoutT (Nullable f)
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Dividend = DividendT Identity

deriving instance Eq Dividend

deriving instance Read Dividend

deriving instance Show Dividend

instance Table DividendT where
  data PrimaryKey DividendT f = DividendPKey (C f
                                              Tezos.OriginatedAddress)
                                           (PrimaryKey DelegateT f) (PrimaryKey CycleT f)
                                deriving (Generic, Beamable)
  primaryKey Dividend {delegator, delegate, cycle} =
    DividendPKey delegator delegate cycle

deriving instance Eq (PrimaryKey DividendT Identity)

deriving instance Read (PrimaryKey DividendT Identity)

deriving instance Show (PrimaryKey DividendT Identity)

data PayoutT f = Payout
  { id :: C f UUID
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Payout = PayoutT Identity

deriving instance Eq Payout

deriving instance Read Payout

deriving instance Show Payout

instance Table PayoutT where
  data PrimaryKey PayoutT f = PayoutId{payoutId :: C f UUID}
                              deriving (Generic, Beamable)
  primaryKey Payout {id} = PayoutId id

deriving instance Eq (PrimaryKey PayoutT Identity)

deriving instance Read (PrimaryKey PayoutT Identity)

deriving instance Show (PrimaryKey PayoutT Identity)

deriving instance Eq (PrimaryKey PayoutT (Nullable Identity))

deriving instance Read (PrimaryKey PayoutT (Nullable Identity))

deriving instance Show (PrimaryKey PayoutT (Nullable Identity))

data RefundT f = Refund
  { payment :: PrimaryKey PaymentT f
  , size :: C f (FixedQty XTZ)
  , payout :: PrimaryKey PayoutT f
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Refund = RefundT Identity

deriving instance Eq Refund

deriving instance Read Refund

deriving instance Show Refund

instance Table RefundT where
  data PrimaryKey RefundT f = RefundPayment (PrimaryKey PaymentT f)
                              deriving (Generic, Beamable)
  primaryKey Refund {payment} = RefundPayment payment

deriving instance Eq (PrimaryKey RefundT Identity)

deriving instance Read (PrimaryKey RefundT Identity)

deriving instance Show (PrimaryKey RefundT Identity)

data DelegationT f = Delegation
  { delegator :: C f Tezos.OriginatedAddress
  , delegate :: PrimaryKey DelegateT f
  , rewardCycle :: PrimaryKey CycleT f
  , size :: C f (FixedQty XTZ)
  , dividend :: PrimaryKey DividendT f
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Delegation = DelegationT Identity

deriving instance Eq Delegation

deriving instance Read Delegation

deriving instance Show Delegation

instance Table DelegationT where
  data PrimaryKey DelegationT f = DelegationPKey (C f
                                                  Tezos.OriginatedAddress)
                                               (PrimaryKey DelegateT f) (PrimaryKey CycleT f)
                                  deriving (Generic, Beamable)
  primaryKey Delegation {delegator, delegate, rewardCycle} =
    DelegationPKey delegator delegate rewardCycle

deriving instance Eq (PrimaryKey DelegationT Identity)

deriving instance Read (PrimaryKey DelegationT Identity)

deriving instance Show (PrimaryKey DelegationT Identity)

-- | Represents payments processed.
data PaymentT f = Payment
  { idx :: C f Word64
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Payment = PaymentT Identity

deriving instance Eq Payment

deriving instance Read Payment

deriving instance Show Payment

instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentIndex (C f Word64)
                               deriving (Generic, Beamable)
  primaryKey Payment {idx} = PaymentIndex idx

deriving instance Eq (PrimaryKey PaymentT Identity)

deriving instance Read (PrimaryKey PaymentT Identity)

deriving instance Show (PrimaryKey PaymentT Identity)

data Schema f = Schema
  { cycles :: f (TableEntity CycleT)
  , delegates :: f (TableEntity DelegateT)
  , rewards :: f (TableEntity RewardT)
  , bills :: f (TableEntity BillT)
  , payouts :: f (TableEntity PayoutT)
  , dividends :: f (TableEntity DividendT)
  , refunds :: f (TableEntity RefundT)
  , payouts :: f (TableEntity PayoutT)
  , delegations :: f (TableEntity DelegationT)
  , payments :: f (TableEntity PaymentT)
  } deriving (Generic)

instance Database Postgres Schema

schema :: DatabaseSettings Postgres Schema
schema = defaultDbSettings

-- TODO: reduce duplication?
selectNextCycle :: Connection -> IO Word64
selectNextCycle conn = do
  let selectMaxCycleNumber =
        select $ aggregate_ max_ $ number <$> all_ (cycles schema)
  m <- runBeamPostgres conn $ runSelectReturningOne selectMaxCycleNumber
  return $
    case m of
      Nothing -> 0
      Just Nothing -> 0
      Just (Just num) -> num + 1

wasCycleAlreadyHandled :: Connection -> Word64 -> IO Bool
wasCycleAlreadyHandled conn cycle = do
  let selectCycle = lookup_ (cycles schema) $ CycleNumber cycle
  runBeamPostgres conn $ isJust <$> runSelectReturningOne selectCycle

delegatesTrackedAtCycle :: Connection -> Word64 -> IO [Tezos.ImplicitAddress]
delegatesTrackedAtCycle conn cycle = do
  let selectDelgates =
        select $
        address <$>
        filter_
          ((val_ cycle >=.) . cycleNumber . firstManagedCycle)
          (all_ $ delegates schema)
  runBeamPostgres conn $ runSelectReturningList selectDelgates

selectNextPayment :: Connection -> IO Word64
selectNextPayment conn = do
  let selectMaxPaymentIdx =
        select $ aggregate_ max_ $ idx <$> all_ (payments schema)
  m <- runBeamPostgres conn $ runSelectReturningOne selectMaxPaymentIdx
  return $
    case m of
      Nothing -> 0
      Just Nothing -> 0
      Just (Just num) -> num + 1

wasPaymentAlreadyHandled :: Connection -> Word64 -> IO Bool
wasPaymentAlreadyHandled conn paymentIdx = do
  let selectPayment = lookup_ (payments schema) $ PaymentIndex paymentIdx
  runBeamPostgres conn $ isJust <$> runSelectReturningOne selectPayment

billsOutstanding :: Connection -> Tezos.ImplicitAddress -> IO [Bill]
billsOutstanding conn delegate_ = do
  let selectBills =
        select $
        orderBy_ (\Bill {timestamp} -> asc_ timestamp) $
        filter_
          (\Bill {delegate, open} ->
             DelegateAddress (val_ delegate_) ==. delegate &&. open ==.
             val_ True) $
        all_ $ bills schema
  runBeamPostgres conn $ runSelectReturningList selectBills

dividendsForBill :: Connection -> UUID -> IO [Dividend]
dividendsForBill conn billId = do
  let selectDividends =
        select $
        filter_ (\Dividend {bill} -> BillId (val_ billId) ==. bill) $
        all_ $ dividends schema
  runBeamPostgres conn $ runSelectReturningList selectDividends

isPlatformDelegate :: Connection -> Tezos.Address -> IO Bool
isPlatformDelegate conn addr = do
  let selectDelegate = lookup_ (delegates schema) $ DelegateAddress $ retag addr
  runBeamPostgres conn $ isJust <$> runSelectReturningOne selectDelegate
