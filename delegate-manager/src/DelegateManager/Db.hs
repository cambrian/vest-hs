module DelegateManager.Db
  ( module DelegateManager.Db
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
    , connectUser = "vest"
    , connectPassword = "localpassword"
    , connectDatabase = "vest"
    }

data UserT (c :: Symbol) (u :: Symbol) f = User
  { _userAddress :: Columnar f (Text' "Address")
  , _userBalance :: Columnar f (Money.Discrete c u)
  } deriving (Generic, Beamable)

type User c u = UserT c u Identity

deriving instance Money.Unit c u => Eq (User c u)

deriving instance Money.Unit c u => Read (User c u)

deriving instance Money.Unit c u => Show (User c u)

instance Money.Unit c u => Table (UserT c u) where
  data PrimaryKey (UserT c u) f = UserAddress (Columnar f
                                               (Text' "Address"))
                                  deriving (Generic, Beamable)
  primaryKey = UserAddress . _userAddress

deriving instance Eq (PrimaryKey (UserT c u) Identity)

deriving instance Read (PrimaryKey (UserT c u) Identity)

deriving instance Show (PrimaryKey (UserT c u) Identity)

data VirtualStakeT (c :: Symbol) (u :: Symbol) f = VirtualStake
  { _virtualStakeId :: Columnar f (Text' "VirtualStakeId")
  , _virtualStakeOwner :: PrimaryKey (UserT c u) f
  , _virtualStakeSize :: Columnar f (Money.Discrete c u)
  , _virtualStakeStartTime :: Columnar f Timestamp
  , _virtualStakeEndTime :: Columnar f Timestamp -- Not storing duration
  , _virtualStakePrice :: Columnar f (Money.Discrete "USD" "cent")
  } deriving (Generic, Beamable)

type VirtualStake c u = VirtualStakeT c u Identity

deriving instance Money.Unit c u => Eq (VirtualStake c u)

deriving instance Money.Unit c u => Read (VirtualStake c u)

deriving instance Money.Unit c u => Show (VirtualStake c u)

instance Money.Unit c u => Table (VirtualStakeT c u) where
  data PrimaryKey (VirtualStakeT c u) f = VirtualStakeId (Columnar f
                                                          (Text' "VirtualStakeId"))
                                          deriving (Generic, Beamable)
  primaryKey = VirtualStakeId . _virtualStakeId

deriving instance Eq (PrimaryKey (VirtualStakeT c u) Identity)

deriving instance Read (PrimaryKey (VirtualStakeT c u) Identity)

deriving instance Show (PrimaryKey (VirtualStakeT c u) Identity)

-- A collection transaction, made on behalf of the owner to an arbitrary recipient address
data CollectionT (c :: Symbol) (u :: Symbol) f = Collection
  { _collectionTxHash :: Columnar f (Text' "TxHash")
  , _collectionOwner :: PrimaryKey (UserT c u) f
  , _collectionRecipient :: Columnar f (Text' "Address")
  , _collectionSize :: Columnar f (Money.Discrete c u)
  , _collectionTime :: Columnar f Timestamp
  } deriving (Generic, Beamable)

type Collection c u = CollectionT c u Identity

deriving instance Money.Unit c u => Eq (Collection c u)

deriving instance Money.Unit c u => Read (Collection c u)

deriving instance Money.Unit c u => Show (Collection c u)

-- instance (KnownSymbol c) => ToJSON (Collection c)
-- instance (KnownSymbol c) => FromJSON (Collection c)
instance Money.Unit c u => Table (CollectionT c u) where
  data PrimaryKey (CollectionT c u) f = CollectionTxHash (Columnar f
                                                          (Text' "TxHash"))
                                        deriving (Generic, Beamable)
  primaryKey = CollectionTxHash . _collectionTxHash

deriving instance Eq (PrimaryKey (CollectionT c u) Identity)

deriving instance Read (PrimaryKey (CollectionT c u) Identity)

deriving instance Show (PrimaryKey (CollectionT c u) Identity)

-- A staking reward event, for a specific staking contract
data StakingRewardT (c :: Symbol) (u :: Symbol) f = StakingReward
  { _stakingRewardId :: Columnar f (Text' "StakingRewardId")
  , _stakingRewardTxHash :: Columnar f (Text' "TxHash") -- ^ the staking reward transaction that this reward is part of
  , _stakingRewardStakingContract :: PrimaryKey (VirtualStakeT c u) f
  , _stakingRewardSize :: Columnar f (Money.Discrete c u)
  , _stakingRewardTime :: Columnar f Timestamp
  } deriving (Generic, Beamable)

type StakingReward c u = StakingRewardT c u Identity

deriving instance Money.Unit c u => Eq (StakingReward c u)

deriving instance Money.Unit c u => Read (StakingReward c u)

deriving instance Money.Unit c u => Show (StakingReward c u)

-- instance (KnownSymbol c) => ToJSON (StakingReward c)
-- instance (KnownSymbol c) => FromJSON (StakingReward c)
instance Money.Unit c u => Table (StakingRewardT c u) where
  data PrimaryKey (StakingRewardT c u) f = StakingRewardId (Columnar
                                                            f
                                                            (Text' "StakingRewardId"))
                                           deriving (Generic, Beamable)
  primaryKey = StakingRewardId . _stakingRewardId

deriving instance Eq (PrimaryKey (StakingRewardT c u) Identity)

deriving instance Read (PrimaryKey (StakingRewardT c u) Identity)

deriving instance Show (PrimaryKey (StakingRewardT c u) Identity)

data CoreDb (c :: Symbol) (u :: Symbol) f = CoreDb
  { _coreUsers :: f (TableEntity (UserT c u))
  , _coreVirtualStakes :: f (TableEntity (VirtualStakeT c u))
  , _coreCollections :: f (TableEntity (VirtualStakeT c u))
  , _coreStakingRewards :: f (TableEntity (StakingRewardT c u))
  } deriving (Generic)

instance Money.Unit c u => Database be (CoreDb c u)

-- Database spec with explicit schema prefixing for the c c
-- such that each table "table" is referred to by "c.table"
coreDb ::
     forall c u be. Money.Unit c u
  => DatabaseSettings be (CoreDb c u)
coreDb =
  defaultDbSettings `withDbModification`
  dbModification
    { _coreUsers = modifyTable prefixCurrencySchema tableModification
    , _coreVirtualStakes = modifyTable prefixCurrencySchema tableModification
    , _coreCollections = modifyTable prefixCurrencySchema tableModification
    } `withDbModification`
  -- for some reason the compiler complains if you put all of these renames into one dbModification
  dbModification
    {_coreStakingRewards = modifyTable prefixCurrencySchema tableModification}
  where
    (Tagged currencyName) = proxyText' (Proxy :: Proxy c)
    prefixCurrencySchema = ((currencyName <> ".") <>)

storeVirtualStake ::
     forall c u. Money.Unit c u
  => Text' "VirtualStakeId"
  -> Text' "Address"
  -> Money.Discrete c u
  -> Time Day
  -> Timestamp
  -> Money.Discrete "USD" "cent"
  -> T
  -> IO ()
-- ^ Inserts owner into c.users with a zero balance if not exists.
storeVirtualStake id ownerAddress size duration startTime price (T conn) = do
  let owner = User @c @u ownerAddress (Money.discrete 0)
      virtualStake =
        VirtualStake
          id
          (pk owner)
          size
          startTime
          (timeAdd duration startTime)
          price
  runBeamPostgres conn $
    runInsert $
    insert
      (_coreUsers coreDb)
      (insertValues [owner])
      (onConflict anyConflict onConflictDoNothing)
  runBeamPostgres conn $
    runInsert $
    insert
      (_coreVirtualStakes coreDb)
      (insertValues [virtualStake])
      onConflictDefault
