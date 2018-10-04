module TezosDelegationCore.Db
  ( module TezosDelegationCore.Db
  ) where

import VestPrelude
import VestPrelude.Db hiding (Connection)
import qualified VestPrelude.Db as Db (Connection)
import qualified VestPrelude.Money as Money

newtype Connection =
  Connection Db.Connection

type instance ResourceConfig Connection = ConnectInfo

instance Resource Connection where
  make cfg = connect cfg >>- Connection
  cleanup (Connection conn) = close conn

localConfig :: ConnectInfo
localConfig =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "delegation-core"
    , connectPassword = "localpassword"
    , connectDatabase = "delegation-core"
    }

data DelegateT f = Delegate
  { _delegateAddress :: C f (Text' "Address")
  , _delegateName :: C f Text
  , _delegateDescription :: C f Text
  , _delegatePriceTiers :: C f (PGArray (Money.Discrete "XTZ" "mutez", Double))
  } deriving (Generic, Beamable)

type Delegate = DelegateT Identity

deriving instance Eq Delegate

deriving instance Read Delegate

deriving instance Show Delegate

instance Table DelegateT where
  data PrimaryKey DelegateT f = DelegateAddress (C f
                                                 (Text' "Address"))
                                deriving (Generic, Beamable)
  primaryKey = DelegateAddress . _delegateAddress

deriving instance Eq (PrimaryKey DelegateT Identity)

deriving instance Read (PrimaryKey DelegateT Identity)

deriving instance Show (PrimaryKey DelegateT Identity)

data CycleT f = Cycle
  { _cycleNumber :: C f (Int64' "CycleNumber")
  , _cycleRightsSnapshotNumber :: C f (Int64' "SnapshotNumber")
  } deriving (Generic, Beamable)

type Cycle = CycleT Identity

deriving instance Eq Cycle

deriving instance Read Cycle

deriving instance Show Cycle

instance Table CycleT where
  data PrimaryKey CycleT f = CycleNumber (C f (Int64' "CycleNumber"))
                             deriving (Generic, Beamable)
  primaryKey = CycleNumber . _cycleNumber

deriving instance Eq (PrimaryKey CycleT Identity)

deriving instance Read (PrimaryKey CycleT Identity)

deriving instance Show (PrimaryKey CycleT Identity)

data RewardT f = Reward
  { _rewardDelegate :: PrimaryKey DelegateT f
  , _rewardRightsCycle :: PrimaryKey CycleT f
  , _rewardDelegatedBalance :: C f (Money.Discrete "XTZ" "mutez")
  , _rewardSizeFromDelegations :: C f (Money.Discrete "XTZ" "mutez")
  , _rewardPaymentOwed :: C f (Money.Discrete "XTZ" "mutez")
  , _rewardPaymentFulfilled :: C f (Money.Discrete "XTZ" "mutez")
  } deriving (Generic, Beamable)

type Reward = RewardT Identity

deriving instance Eq Reward

deriving instance Show Reward

deriving instance Read Reward

instance Table RewardT where
  data PrimaryKey RewardT
       f = RewardDelegateAndRightsCycle (PrimaryKey DelegateT f)
                                        (PrimaryKey CycleT f)
             deriving (Generic, Beamable)
  primaryKey =
    RewardDelegateAndRightsCycle <$> _rewardDelegate <*> _rewardRightsCycle

deriving instance Eq (PrimaryKey RewardT Identity)

deriving instance Read (PrimaryKey RewardT Identity)

deriving instance Show (PrimaryKey RewardT Identity)

data PaymentT f = Payment
  { _paymentTxHash :: C f (Text' "TxHash")
  , _paymentDelegate :: PrimaryKey DelegateT f
  , _paymentSize :: C f (Money.Discrete "XTZ" "mutez")
  , _paymentTime :: C f Timestamp
  , _paymentStatus :: C f TxStatus
  } deriving (Generic, Beamable)

type Payment = PaymentT Identity

deriving instance Eq Payment

deriving instance Show Payment

deriving instance Read Payment

instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentTxHash (C f (Text' "TxHash"))
                               deriving (Generic, Beamable)
  primaryKey = PaymentTxHash . _paymentTxHash

deriving instance Eq (PrimaryKey PaymentT Identity)

deriving instance Read (PrimaryKey PaymentT Identity)

deriving instance Show (PrimaryKey PaymentT Identity)

data PayoutT f = Payout
  { _payoutTxHash :: C f (Text' "TxHash")
  , _payoutDelegator :: C f (Text' "Address")
  , _payoutSize :: C f (Money.Discrete "XTZ" "mutez")
  , _payoutTime :: C f Timestamp
  , _payoutStatus :: C f TxStatus
  } deriving (Generic, Beamable)

type Payout = PayoutT Identity

deriving instance Eq Payout

deriving instance Show Payout

deriving instance Read Payout

instance Table PayoutT where
  data PrimaryKey PayoutT f = PayoutTxHash (C f (Text' "TxHash"))
                              deriving (Generic, Beamable)
  primaryKey = PayoutTxHash . _payoutTxHash

deriving instance Eq (PrimaryKey PayoutT Identity)

deriving instance Read (PrimaryKey PayoutT Identity)

deriving instance Show (PrimaryKey PayoutT Identity)

data DelegationT f = Delegation
  { _delegationDelegator :: C f (Text' "Address")
  , _delegationReward :: PrimaryKey RewardT f
  , _delegationSize :: C f (Money.Discrete "XTZ" "mutez")
  , _delegationDividend :: C f (Money.Discrete "XTZ" "mutez")
  , _delegationPayout :: PrimaryKey PayoutT f
  } deriving (Generic, Beamable)

type Delegation = DelegationT Identity

deriving instance Eq Delegation

deriving instance Show Delegation

deriving instance Read Delegation

instance Table DelegationT where
  data PrimaryKey DelegationT f = DelegationDelegatorAndReward (C f
                                                                (Text' "Address"))
                                                             (PrimaryKey RewardT f)
                                  deriving (Generic, Beamable)
  primaryKey =
    DelegationDelegatorAndReward <$> _delegationDelegator <*> _delegationReward

deriving instance Eq (PrimaryKey DelegationT Identity)

deriving instance Read (PrimaryKey DelegationT Identity)

deriving instance Show (PrimaryKey DelegationT Identity)

data TezosDelegationCoreDb f = TezosDelegationCoreDb
  { _tezosDelegationCoreDelegates :: f (TableEntity DelegateT)
  , _tezosDelegationCoreCycles :: f (TableEntity CycleT)
  , _tezosDelegationCoreRewards :: f (TableEntity RewardT)
  , _tezosDelegationCorePayments :: f (TableEntity PaymentT)
  , _tezosDelegationCorePayouts :: f (TableEntity PayoutT)
  , _tezosDelegationCoreDelegations :: f (TableEntity DelegationT)
  } deriving (Generic)

instance Database be TezosDelegationCoreDb

-- Database spec with explicit schema prefixing for the c c
-- such that each table "table" is referred to by "c.table"
t :: DatabaseSettings be TezosDelegationCoreDb
t = defaultDbSettings
-- storeVirtualStake ::
--      forall c u. Money.Unit c u
--   => Text' "VirtualStakeId"
--   -> Text' "Address"
--   -> Money.Discrete c u
--   -> Time Day
--   -> Timestamp
--   -> Money.Discrete "USD" "cent"
--   -> T
--   -> IO ()
-- -- ^ Inserts owner into c.users with a zero balance if not exists.
-- storeVirtualStake id ownerAddress size duration startTime price (T conn) = do
--   let owner = User @c @u ownerAddress (Money.discrete 0)
--       virtualStake =
--         VirtualStake
--           id
--           (pk owner)
--           size
--           startTime
--           (timeAdd duration startTime)
--           price
--   runBeamPostgres conn $
--     runInsert $
--     insert
--       (_coreUsers coreDb)
--       (insertValues [owner])
--       (onConflict anyConflict onConflictDoNothing)
--   runBeamPostgres conn $
--     runInsert $
--     insert
--       (_coreVirtualStakes coreDb)
--       (insertValues [virtualStake])
--       onConflictDefault
