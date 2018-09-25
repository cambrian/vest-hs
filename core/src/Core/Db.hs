module Core.Db
  ( module Core.Db
  ) where

import qualified Database.PostgreSQL.Simple as Pg
import VestPrelude
import VestPrelude.Db
import qualified VestPrelude.Money as Money

newtype T =
  T Pg.Connection

type instance ResourceConfig T = Pg.ConnectInfo

instance Resource T where
  hold cfg = Pg.connect cfg >>- T
  release (T conn) = Pg.close conn

-- TODO: could be better to use Money.Discrete
data UserT (currency :: Symbol) f = User
  { _userAddress :: Columnar f (Id "Address")
  , _userBalance :: Columnar f (Money.Dense currency)
  } deriving (Generic, Beamable)

type User c = UserT c Identity

deriving instance Eq (User c)

deriving instance (KnownSymbol c) => Read (User c)

deriving instance (KnownSymbol c) => Show (User c)

instance (KnownSymbol c) => ToJSON (User c)

instance (KnownSymbol c) => FromJSON (User c)

instance (KnownSymbol c) => Table (UserT c) where
  data PrimaryKey (UserT c) f = UserAddress (Columnar f
                                             (Id "Address"))
                                deriving (Generic, Beamable)
  primaryKey = UserAddress . _userAddress

deriving instance Eq (PrimaryKey (UserT c) Identity)

deriving instance Read (PrimaryKey (UserT c) Identity)

deriving instance Show (PrimaryKey (UserT c) Identity)

data VirtualStakeT (currency :: Symbol) f = VirtualStake
  { _virtualStakeId :: Columnar f (Id "VirtualStake")
  , _virtualStakeOwner :: PrimaryKey (UserT currency) f
  , _virtualStakeSize :: Columnar f (Money.Dense currency)
  , _virtualStakeStartTime :: Columnar f Timestamp
  , _virtualStakeEndTime :: Columnar f Timestamp -- Not storing duration
  , _virtualStakePrice :: Columnar f (Money.Discrete "USD" "cent")
  } deriving (Generic, Beamable)

type VirtualStake c = VirtualStakeT c Identity

deriving instance Eq (VirtualStake c)

deriving instance (KnownSymbol c) => Read (VirtualStake c)

deriving instance (KnownSymbol c) => Show (VirtualStake c)

-- can't add these because o-clock's aeson flag isn't being set properly
-- instance (KnownSymbol c) => ToJSON (VirtualStake c)
-- instance (KnownSymbol c) => FromJSON (VirtualStake c)
instance (KnownSymbol c) => Table (VirtualStakeT c) where
  data PrimaryKey (VirtualStakeT c) f = VirtualStakeId (Columnar f
                                                        (Id "VirtualStake"))
                                        deriving (Generic, Beamable)
  primaryKey = VirtualStakeId . _virtualStakeId

deriving instance Eq (PrimaryKey (VirtualStakeT c) Identity)

deriving instance Read (PrimaryKey (VirtualStakeT c) Identity)

deriving instance Show (PrimaryKey (VirtualStakeT c) Identity)

-- A collection transaction, made on behalf of the owner to an arbitrary recipient address
data CollectionT (currency :: Symbol) f = Collection
  { _collectionTxHash :: Columnar f (Id "TxHash")
  , _collectionOwner :: PrimaryKey (UserT currency) f
  , _collectionRecipient :: Columnar f (Id "Address")
  , _collectionSize :: Columnar f (Money.Dense currency)
  , _collectionTime :: Columnar f Timestamp
  } deriving (Generic, Beamable)

type Collection c = CollectionT c Identity

deriving instance Eq (Collection c)

deriving instance (KnownSymbol c) => Read (Collection c)

deriving instance (KnownSymbol c) => Show (Collection c)

-- instance (KnownSymbol c) => ToJSON (Collection c)
-- instance (KnownSymbol c) => FromJSON (Collection c)
instance (KnownSymbol c) => Table (CollectionT c) where
  data PrimaryKey (CollectionT c) f = CollectionTxHash (Columnar f
                                                        (Id "TxHash"))
                                      deriving (Generic, Beamable)
  primaryKey = CollectionTxHash . _collectionTxHash

deriving instance Eq (PrimaryKey (CollectionT c) Identity)

deriving instance Read (PrimaryKey (CollectionT c) Identity)

deriving instance Show (PrimaryKey (CollectionT c) Identity)

-- A staking reward event, for a specific staking contract
data StakingRewardT (currency :: Symbol) f = StakingReward
  { _stakingRewardId :: Columnar f (Id "StakingReward")
  , _stakingRewardTxHash :: Columnar f (Id "TxHash") -- ^ the staking reward transaction that this reward is part of
  , _stakingRewardStakingContract :: PrimaryKey (VirtualStakeT currency) f
  , _stakingRewardSize :: Columnar f (Money.Dense currency)
  , _stakingRewardTime :: Columnar f Timestamp
  } deriving (Generic, Beamable)

type StakingReward c = StakingRewardT c Identity

deriving instance Eq (StakingReward c)

deriving instance (KnownSymbol c) => Read (StakingReward c)

deriving instance (KnownSymbol c) => Show (StakingReward c)

-- instance (KnownSymbol c) => ToJSON (StakingReward c)
-- instance (KnownSymbol c) => FromJSON (StakingReward c)
instance (KnownSymbol c) => Table (StakingRewardT c) where
  data PrimaryKey (StakingRewardT c) f = StakingRewardId (Columnar f
                                                          (Id "StakingReward"))
                                         deriving (Generic, Beamable)
  primaryKey = StakingRewardId . _stakingRewardId

deriving instance Eq (PrimaryKey (StakingRewardT c) Identity)

deriving instance Read (PrimaryKey (StakingRewardT c) Identity)

deriving instance Show (PrimaryKey (StakingRewardT c) Identity)

data CoreDb (currency :: Symbol) f = CoreDb
  { _coreUsers :: f (TableEntity (UserT currency))
  , _coreVirtualStakes :: f (TableEntity (VirtualStakeT currency))
  , _coreCollections :: f (TableEntity (VirtualStakeT currency))
  , _coreStakingRewards :: f (TableEntity (StakingRewardT currency))
  } deriving (Generic)

instance (KnownSymbol c) => Database be (CoreDb c)

-- Database spec with explicit schema prefixing for the currency c
-- each table "table" is referred to by "c.table"
coreDb :: (KnownSymbol c) => Proxy c -> DatabaseSettings be (CoreDb c)
coreDb pxy =
  defaultDbSettings `withDbModification`
  dbModification
    { _coreUsers = modifyTable prefixCurrencySchema tableModification
    , _coreVirtualStakes = modifyTable prefixCurrencySchema tableModification
    , _coreCollections = modifyTable prefixCurrencySchema tableModification
    } `withDbModification` -- for some reason you can't put all of these renames together
  dbModification
    {_coreStakingRewards = modifyTable prefixCurrencySchema tableModification}
  where
    prefixCurrencySchema = ((proxyText pxy <> ".") <>)
