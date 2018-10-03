module TezosDelegationCore.Db
  ( module TezosDelegationCore.Db
  ) where

import VestPrelude
import VestPrelude.Db
import qualified VestPrelude.Money as Money

newtype T =
  T Connection

type instance ResourceConfig T = ConnectInfo

instance Resource T where
  make cfg = connect cfg >>- T
  cleanup (T conn) = close conn

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
  { _delegateAddress :: Columnar f (Text' "Address")
  , _delegateName :: Columnar f Text
  , _delegateDescription :: Columnar f Text
  , _delegatePriceTiers :: Columnar f (PGArray ( Money.Discrete "XTZ" "mutez"
                                               , Double))
  } deriving (Generic, Beamable)

type Delegate = DelegateT Identity

deriving instance Eq Delegate

deriving instance Read Delegate

deriving instance Show Delegate

instance Table DelegateT where
  data PrimaryKey DelegateT f = DelegateAddress (Columnar f
                                                 (Text' "Address"))
                                deriving (Generic, Beamable)
  primaryKey = DelegateAddress . _delegateAddress

deriving instance Eq (PrimaryKey DelegateT Identity)

deriving instance Read (PrimaryKey DelegateT Identity)

deriving instance Show (PrimaryKey DelegateT Identity)

data CycleT f = Cycle
  { _cycleNumber :: Columnar f (Int64' "CycleNumber")
  , _cycleRightsSnapshotNumber :: Columnar f (Int64' "SnapshotNumber")
  } deriving (Generic, Beamable)

type Cycle = CycleT Identity

deriving instance Eq Cycle

deriving instance Read Cycle

deriving instance Show Cycle

instance Table CycleT where
  data PrimaryKey CycleT f = CycleNumber (Columnar f
                                          (Int64' "CycleNumber"))
                             deriving (Generic, Beamable)
  primaryKey = CycleNumber . _cycleNumber

deriving instance Eq (PrimaryKey CycleT Identity)

deriving instance Read (PrimaryKey CycleT Identity)

deriving instance Show (PrimaryKey CycleT Identity)

data RewardT f = Reward
  { _rewardDelegate :: PrimaryKey DelegateT f
  , _rewardRightsCycle :: PrimaryKey CycleT f
  , _rewardDelegatedBalance :: Columnar f (Money.Discrete "XTZ" "mutez")
  , _rewardSizeFromDelegations :: Columnar f (Money.Discrete "XTZ" "mutez")
  , _rewardPaymentOwed :: Columnar f (Money.Discrete "XTZ" "mutez")
  , _rewardPaymentFulfilled :: Columnar f (Money.Discrete "XTZ" "mutez")
  } deriving (Generic, Beamable)

type Reward = RewardT Identity

deriving instance Eq Reward

deriving instance Show Reward

deriving instance Read Reward
-- data VirtualStakeT (c :: Symbol) (u :: Symbol) f = VirtualStake
--   { _virtualStakeId :: Columnar f (Text' "VirtualStakeId")
--   , _virtualStakeOwner :: PrimaryKey (DelegateT) f
--   , _virtualStakeSize :: Columnar f (Money.Discrete c u)
--   , _virtualStakeStartTime :: Columnar f Timestamp
--   , _virtualStakeEndTime :: Columnar f Timestamp -- Not storing duration
--   , _virtualStakePrice :: Columnar f (Money.Discrete "USD" "cent")
--   } deriving (Generic, Beamable)
-- type VirtualStake c u = VirtualStakeT c u Identity
-- deriving instance Money.Unit c u => Eq (VirtualStake c u)
-- deriving instance Money.Unit c u => Read (VirtualStake c u)
-- deriving instance Money.Unit c u => Show (VirtualStake c u)
-- instance Money.Unit c u => Table (VirtualStakeT c u) where
--   data PrimaryKey (VirtualStakeT c u) f = VirtualStakeId (Columnar f
--                                                           (Text' "VirtualStakeId"))
--                                           deriving (Generic, Beamable)
--   primaryKey = VirtualStakeId . _virtualStakeId
-- deriving instance Eq (PrimaryKey (VirtualStakeT c u) Identity)
-- deriving instance Read (PrimaryKey (VirtualStakeT c u) Identity)
-- deriving instance Show (PrimaryKey (VirtualStakeT c u) Identity)
-- -- A collection transaction, made on behalf of the owner to an arbitrary recipient address
-- data CollectionT (c :: Symbol) (u :: Symbol) f = Collection
--   { _collectionTxHash :: Columnar f (Text' "TxHash")
--   , _collectionOwner :: PrimaryKey (DelegateT) f
--   , _collectionRecipient :: Columnar f (Text' "Address")
--   , _collectionSize :: Columnar f (Money.Discrete c u)
--   , _collectionTime :: Columnar f Timestamp
--   } deriving (Generic, Beamable)
-- type Collection c u = CollectionT c u Identity
-- deriving instance Money.Unit c u => Eq (Collection c u)
-- deriving instance Money.Unit c u => Read (Collection c u)
-- deriving instance Money.Unit c u => Show (Collection c u)
-- -- instance (KnownSymbol c) => ToJSON (Collection c)
-- -- instance (KnownSymbol c) => FromJSON (Collection c)
-- instance Money.Unit c u => Table (CollectionT c u) where
--   data PrimaryKey (CollectionT c u) f = CollectionTxHash (Columnar f
--                                                           (Text' "TxHash"))
--                                         deriving (Generic, Beamable)
--   primaryKey = CollectionTxHash . _collectionTxHash
-- deriving instance Eq (PrimaryKey (CollectionT c u) Identity)
-- deriving instance Read (PrimaryKey (CollectionT c u) Identity)
-- deriving instance Show (PrimaryKey (CollectionT c u) Identity)
-- -- A staking reward event, for a specific staking contract
-- data StakingRewardT (c :: Symbol) (u :: Symbol) f = StakingReward
--   { _stakingRewardId :: Columnar f (Text' "StakingRewardId")
--   , _stakingRewardTxHash :: Columnar f (Text' "TxHash") -- ^ the staking reward transaction that this reward is part of
--   , _stakingRewardStakingContract :: PrimaryKey (VirtualStakeT c u) f
--   , _stakingRewardSize :: Columnar f (Money.Discrete c u)
--   , _stakingRewardTime :: Columnar f Timestamp
--   } deriving (Generic, Beamable)
-- type StakingReward c u = StakingRewardT c u Identity
-- deriving instance Money.Unit c u => Eq (StakingReward c u)
-- deriving instance Money.Unit c u => Read (StakingReward c u)
-- deriving instance Money.Unit c u => Show (StakingReward c u)
-- -- instance (KnownSymbol c) => ToJSON (StakingReward c)
-- -- instance (KnownSymbol c) => FromJSON (StakingReward c)
-- instance Money.Unit c u => Table (StakingRewardT c u) where
--   data PrimaryKey (StakingRewardT c u) f = StakingRewardId (Columnar
--                                                             f
--                                                             (Text' "StakingRewardId"))
--                                            deriving (Generic, Beamable)
--   primaryKey = StakingRewardId . _stakingRewardId
-- deriving instance Eq (PrimaryKey (StakingRewardT c u) Identity)
-- deriving instance Read (PrimaryKey (StakingRewardT c u) Identity)
-- deriving instance Show (PrimaryKey (StakingRewardT c u) Identity)
-- data CoreDb (c :: Symbol) (u :: Symbol) f = CoreDb
--   { _coreUsers :: f (TableEntity (DelegateT))
--   , _coreVirtualStakes :: f (TableEntity (VirtualStakeT c u))
--   , _coreCollections :: f (TableEntity (VirtualStakeT c u))
--   , _coreStakingRewards :: f (TableEntity (StakingRewardT c u))
--   } deriving (Generic)
-- instance Money.Unit c u => Database be (CoreDb c u)
-- -- Database spec with explicit schema prefixing for the c c
-- -- such that each table "table" is referred to by "c.table"
-- coreDb ::
--      forall c u be. Money.Unit c u
--   => DatabaseSettings be (CoreDb c u)
-- coreDb =
--   defaultDbSettings `withDbModification`
--   dbModification
--     { _coreUsers = modifyTable prefixCurrencySchema tableModification
--     , _coreVirtualStakes = modifyTable prefixCurrencySchema tableModification
--     , _coreCollections = modifyTable prefixCurrencySchema tableModification
--     } `withDbModification`
--   -- for some reason the compiler complains if you put all of these renames into one dbModification
--   dbModification
--     {_coreStakingRewards = modifyTable prefixCurrencySchema tableModification}
--   where
--     (Tagged currencyName) = proxyText' (Proxy :: Proxy c)
--     prefixCurrencySchema = ((currencyName <> ".") <>)
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
