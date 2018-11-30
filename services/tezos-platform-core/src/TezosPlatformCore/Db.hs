module TezosPlatformCore.Db
  ( module TezosPlatformCore.Db
  ) where

import Postgres
import qualified Tezos
import qualified Tezos.Rpc as Tezos
import Vest

data BlockT f = Block
  { number :: C f Word64
  , time :: C f Time
  , created_at :: C f Time
  , handled :: C f Bool
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockNumber{blockNumber :: C f Word64}
                             deriving (Generic, Beamable)
  primaryKey Block {number} = BlockNumber number

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

instance IndexableTable BlockT where
  indexColumn = number

data CycleT f = Cycle
  { number :: C f Word64
  , first_block :: PrimaryKey BlockT f
  , created_at :: C f Time
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

instance IndexableTable CycleT where
  indexColumn = number

data DelegateT f = Delegate
  { address :: C f Tezos.ImplicitAddress
  , dummy_address :: C f Tezos.ImplicitAddress
  , name :: C f Text
  , description :: C f Text
  , first_managed_block :: PrimaryKey BlockT f
  , price_tiers :: C f (PgJSON [(FixedQty XTZ, Rational)])
  , created_at :: C f Time
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

-- | Not accepting partial payments at the moment.
data RewardT f = Reward
  { cycle :: PrimaryKey CycleT f
  , delegate :: PrimaryKey DelegateT f
  , size :: C f (FixedQty XTZ)
  , staking_balance :: C f (FixedQty XTZ)
  , delegated_balance :: C f (FixedQty XTZ)
  , payment_owed :: C f (FixedQty XTZ)
  , payment :: PrimaryKey PaymentT (Nullable f)
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Reward = RewardT Identity

deriving instance Eq Reward

deriving instance Read Reward

deriving instance Show Reward

instance Table RewardT where
  data PrimaryKey RewardT f = RewardPKey (PrimaryKey CycleT f)
                                       (PrimaryKey DelegateT f)
                              deriving (Generic, Beamable)
  primaryKey Reward {cycle, delegate} = RewardPKey cycle delegate

deriving instance Eq (PrimaryKey RewardT Identity)

deriving instance Read (PrimaryKey RewardT Identity)

deriving instance Show (PrimaryKey RewardT Identity)

data DividendT f = Dividend
  { reward :: PrimaryKey RewardT f
  , delegator :: C f Tezos.OriginatedAddress
  , size :: C f (FixedQty XTZ)
  , payout :: PrimaryKey PayoutT (Nullable f)
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Dividend = DividendT Identity

deriving instance Eq Dividend

deriving instance Read Dividend

deriving instance Show Dividend

instance Table DividendT where
  data PrimaryKey DividendT f = DividendPKey (PrimaryKey RewardT f)
                                           (C f Tezos.OriginatedAddress)
                                deriving (Generic, Beamable)
  primaryKey Dividend {reward, delegator} = DividendPKey reward delegator

deriving instance Eq (PrimaryKey DividendT Identity)

deriving instance Read (PrimaryKey DividendT Identity)

deriving instance Show (PrimaryKey DividendT Identity)

-- All payouts from Vest to other people. Includes e.g. dividends and refunds
data PayoutT f = Payout
  { id :: C f UUID
  , to :: C f Tezos.Address
  , size :: C f (FixedQty XTZ)
  , fee :: C f (FixedQty XTZ)
  , created_at :: C f Time
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

-- Represents delegations attributable to vest. May also be necessary for tracking
-- overdelegation...
data DelegationT f = Delegation
  { delegate :: PrimaryKey DelegateT f
  , delegator :: C f Tezos.OriginatedAddress
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Delegation = DelegationT Identity

deriving instance Eq Delegation

deriving instance Read Delegation

deriving instance Show Delegation

instance Table DelegationT where
  data PrimaryKey DelegationT f = DelegationPKey (PrimaryKey
                                                  DelegateT
                                                  f)
                                               (C f Tezos.OriginatedAddress)
                                  deriving (Generic, Beamable)
  primaryKey Delegation {delegate, delegator} =
    DelegationPKey delegate delegator

deriving instance Eq (PrimaryKey DelegationT Identity)

deriving instance Read (PrimaryKey DelegationT Identity)

deriving instance Show (PrimaryKey DelegationT Identity)

-- | Payments from bakers.
data PaymentT f = Payment
  { hash :: C f Tezos.OperationHash
  , from :: C f Tezos.Address
  , size :: C f (FixedQty XTZ)
  , block :: PrimaryKey BlockT f
  , refund :: PrimaryKey PayoutT (Nullable f)
  , created_at :: C f Time
  } deriving (Generic, Beamable)

type Payment = PaymentT Identity

deriving instance Eq Payment

deriving instance Read Payment

deriving instance Show Payment

instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentHash (C f Tezos.OperationHash)
                               deriving (Generic, Beamable)
  primaryKey Payment {hash} = PaymentHash hash

deriving instance Eq (PrimaryKey PaymentT Identity)

deriving instance Read (PrimaryKey PaymentT Identity)

deriving instance Show (PrimaryKey PaymentT Identity)

deriving instance Eq (PrimaryKey PaymentT (Nullable Identity))

deriving instance Read (PrimaryKey PaymentT (Nullable Identity))

deriving instance Show (PrimaryKey PaymentT (Nullable Identity))

data Schema f = Schema
  { blocks :: f (TableEntity BlockT)
  , cycles :: f (TableEntity CycleT)
  , delegates :: f (TableEntity DelegateT)
  , rewards :: f (TableEntity RewardT)
  , payouts :: f (TableEntity PayoutT)
  , dividends :: f (TableEntity DividendT)
  , delegations :: f (TableEntity DelegationT)
  , payments :: f (TableEntity PaymentT)
  } deriving (Generic)

instance Database Postgres Schema

checkedSchema :: CheckedDatabaseSettings Postgres Schema
checkedSchema = defaultMigratableDbSettings @PgCommandSyntax

schema :: DatabaseSettings Postgres Schema
schema = unCheckDatabase checkedSchema

selectNextUnhandledBlock :: Pg Word64
-- Assumes that we don't have any gaps in handled blocks.
selectNextUnhandledBlock = do
  let number Block {number} = number
  m <-
    runSelectReturningOne $
    select $
    number <$>
    limit_
      1
      (orderBy_ (desc_ . number) $
       filter_ (\Block {handled} -> handled ==. val_ True) $
       all_ $ blocks schema)
  return $
    case m of
      Nothing -> fromIntegral Tezos.firstReadableBlockNumber
      Just n -> n + 1

delegatesTrackedAtBlock :: Word64 -> Pg [Tezos.ImplicitAddress]
delegatesTrackedAtBlock block =
  runSelectReturningList $
  select $
  address <$>
  filter_
    ((val_ block >=.) . blockNumber . first_managed_block)
    (all_ $ delegates schema)

wasRewardProcessed :: Word64 -> Tezos.ImplicitAddress -> Pg Bool
wasRewardProcessed cycle delegate =
  isJust <$>
  runSelectReturningOne
    (lookup_ (rewards schema) $
     RewardPKey (CycleNumber cycle) (DelegateAddress delegate))

rewardsUnpaid :: Tezos.ImplicitAddress -> Pg [Reward]
rewardsUnpaid delegate_ =
  runSelectReturningList $
  select $
  orderBy_ (\Reward {created_at} -> asc_ created_at) $
  filter_
    (\Reward {delegate, payment} ->
       DelegateAddress (val_ delegate_) ==. delegate &&. payment ==.
       val_ (PaymentHash Nothing)) $
  all_ $ rewards schema

dividendsForReward :: Reward -> Pg [Dividend]
-- ^ Not possible to batch this query because beam does not support composite types
-- see: https://github.com/tathougies/beam/issues/309
dividendsForReward reward_ =
  runSelectReturningList $
  select $
  filter_ (\Dividend {reward} -> reward ==. val_ (pk reward_)) $
  all_ $ dividends schema

isPlatformDelegate :: Tezos.Address -> Pg Bool
isPlatformDelegate addr =
  isJust <$>
  runSelectReturningOne
    (lookup_ (delegates schema) $ DelegateAddress $ Tagged addr)

isPlatformDelegation ::
     Tezos.ImplicitAddress -> Tezos.OriginatedAddress -> Pg Bool
isPlatformDelegation delegate delegator =
  isJust <$>
  runSelectReturningOne
    (lookup_ (delegations schema) $
     DelegationPKey (DelegateAddress delegate) delegator)

getDelegatePriceTiers :: Tezos.ImplicitAddress -> Pg [(FixedQty XTZ, Rational)]
getDelegatePriceTiers addr = do
  m <- runSelectReturningOne $ lookup_ (delegates schema) $ DelegateAddress addr
  case m of
    Nothing -> liftIO $ throw BugException
    Just Delegate {price_tiers = PgJSON tiers} -> return tiers

wasPaymentHandled :: Tezos.OperationHash -> Pg Bool
wasPaymentHandled hash =
  isJust <$>
  runSelectReturningOne (lookup_ (payments schema) $ PaymentHash hash)

delegateOfDummyAddress ::
     Tezos.ImplicitAddress -> Pg (Maybe Tezos.ImplicitAddress)
delegateOfDummyAddress addr =
  runSelectReturningOne $
  select $
  address <$>
  filter_
    (\Delegate {dummy_address} -> dummy_address ==. val_ addr)
    (all_ $ delegates schema)
