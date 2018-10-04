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
  { address :: C f (Text' "Address")
  , name :: C f Text
  , description :: C f Text
  , priceTiers :: C f (PGArray (Money.XTZ, Double))
  } deriving (Generic, Beamable)

type Delegate = DelegateT Identity

deriving instance Eq Delegate

deriving instance Read Delegate

deriving instance Show Delegate

instance Table DelegateT where
  data PrimaryKey DelegateT f = DelegateAddress (C f
                                                 (Text' "Address"))
                                deriving (Generic, Beamable)
  primaryKey = DelegateAddress . address

deriving instance Eq (PrimaryKey DelegateT Identity)

deriving instance Read (PrimaryKey DelegateT Identity)

deriving instance Show (PrimaryKey DelegateT Identity)

data CycleT f = Cycle
  { number :: C f (Int64' "CycleNumber")
  , timestamp :: C f Timestamp
  } deriving (Generic, Beamable)

type Cycle = CycleT Identity

deriving instance Eq Cycle

deriving instance Read Cycle

deriving instance Show Cycle

instance Table CycleT where
  data PrimaryKey CycleT f = CycleNumber (C f (Int64' "CycleNumber"))
                             deriving (Generic, Beamable)
  primaryKey = CycleNumber . number

deriving instance Eq (PrimaryKey CycleT Identity)

deriving instance Read (PrimaryKey CycleT Identity)

deriving instance Show (PrimaryKey CycleT Identity)

data RewardT f = Reward
  { delegate :: PrimaryKey DelegateT f
  , rightsCycle :: PrimaryKey CycleT f
  , delegatedBalance :: C f Money.XTZ
  , sizeFromDelegations :: C f Money.XTZ
  , paymentOwed :: C f Money.XTZ
  , paymentFulfilled :: C f Money.XTZ
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
  primaryKey Reward {delegate, rightsCycle} =
    RewardDelegateAndRightsCycle delegate rightsCycle

deriving instance Eq (PrimaryKey RewardT Identity)

deriving instance Read (PrimaryKey RewardT Identity)

deriving instance Show (PrimaryKey RewardT Identity)

data PaymentT f = Payment
  { txHash :: C f (Text' "TxHash")
  , delegate :: PrimaryKey DelegateT f
  , size :: C f Money.XTZ
  , time :: C f Timestamp
  , status :: C f TxStatus
  } deriving (Generic, Beamable)

type Payment = PaymentT Identity

deriving instance Eq Payment

deriving instance Show Payment

deriving instance Read Payment

instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentTxHash (C f (Text' "TxHash"))
                               deriving (Generic, Beamable)
  primaryKey Payment {txHash} = PaymentTxHash txHash

deriving instance Eq (PrimaryKey PaymentT Identity)

deriving instance Read (PrimaryKey PaymentT Identity)

deriving instance Show (PrimaryKey PaymentT Identity)

data PayoutT f = Payout
  { txHash :: C f (Text' "TxHash")
  , delegator :: C f (Text' "Address")
  , size :: C f Money.XTZ
  , time :: C f Timestamp
  , status :: C f TxStatus
  } deriving (Generic, Beamable)

type Payout = PayoutT Identity

deriving instance Eq Payout

deriving instance Show Payout

deriving instance Read Payout

instance Table PayoutT where
  data PrimaryKey PayoutT f = PayoutTxHash (C f (Text' "TxHash"))
                              deriving (Generic, Beamable)
  primaryKey Payout {txHash} = PayoutTxHash txHash

deriving instance Eq (PrimaryKey PayoutT Identity)

deriving instance Read (PrimaryKey PayoutT Identity)

deriving instance Show (PrimaryKey PayoutT Identity)

data DelegationT f = Delegation
  { delegator :: C f (Text' "Address")
  , reward :: PrimaryKey RewardT f
  , size :: C f Money.XTZ
  , dividend :: C f Money.XTZ
  , payout :: PrimaryKey PayoutT f
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
  primaryKey Delegation {delegator, reward} =
    DelegationDelegatorAndReward delegator reward

deriving instance Eq (PrimaryKey DelegationT Identity)

deriving instance Read (PrimaryKey DelegationT Identity)

deriving instance Show (PrimaryKey DelegationT Identity)

data T f = T
  { delegates :: f (TableEntity DelegateT)
  , cycles :: f (TableEntity CycleT)
  , rewards :: f (TableEntity RewardT)
  , payments :: f (TableEntity PaymentT)
  , payouts :: f (TableEntity PayoutT)
  , delegations :: f (TableEntity DelegationT)
  } deriving (Generic)

instance Database be T

-- Database spec with explicit schema prefixing for the c c
-- such that each table "table" is referred to by "c.table"
t :: DatabaseSettings be T
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
--       (Users coreDb)
--       (insertValues [owner])
--       (onConflict anyConflict onConflictDoNothing)
--   runBeamPostgres conn $
--     runInsert $
--     insert
--       (VirtualStakes coreDb)
--       (insertValues [virtualStake])
--       onConflictDefault
