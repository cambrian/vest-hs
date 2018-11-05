module TezosDelegationCore.Db
  ( module TezosDelegationCore.Db
  ) where

import Db
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
  data PrimaryKey CycleT f = CycleNumber (C f Word64)
                             deriving (Generic, Beamable)
  primaryKey Cycle {number} = CycleNumber number

deriving instance Eq (PrimaryKey CycleT Identity)

deriving instance Read (PrimaryKey CycleT Identity)

deriving instance Show (PrimaryKey CycleT Identity)

data DelegateT f = Delegate
  { address :: C f (Text' "Address")
  , name :: C f Text
  , description :: C f Text
  , firstManagedCycle :: PrimaryKey CycleT f
  , priceTiers :: C f [(FixedQty "XTZ", Rational)]
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Delegate = DelegateT Identity

deriving instance Eq Delegate

deriving instance Read Delegate

deriving instance Show Delegate

instance Table DelegateT where
  data PrimaryKey DelegateT f = DelegateAddress (C f
                                                 (Text' "Address"))
                                deriving (Generic, Beamable)
  primaryKey Delegate {address} = DelegateAddress address

deriving instance Eq (PrimaryKey DelegateT Identity)

deriving instance Read (PrimaryKey DelegateT Identity)

deriving instance Show (PrimaryKey DelegateT Identity)

-- TODO: do we also need to store total reward, total balance?
data RewardT f = Reward
  { delegate :: PrimaryKey DelegateT f
  , rightsCycle :: PrimaryKey CycleT f
  , delegatedBalance :: C f (FixedQty "XTZ")
  , rewardFromDelegations :: C f (FixedQty "XTZ")
  , owedToVest :: C f (FixedQty "XTZ")
  , paymentFulfilled :: C f (FixedQty "XTZ")
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
  primaryKey Reward {delegate, rightsCycle} =
    RewardDelegateAndCycle delegate rightsCycle

deriving instance Eq (PrimaryKey RewardT Identity)

deriving instance Read (PrimaryKey RewardT Identity)

deriving instance Show (PrimaryKey RewardT Identity)

data PayoutT f = Payout
  { id :: C f UUID
  , delegator :: C f (Text' "Address")
  , size :: C f (FixedQty "XTZ")
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Payout = PayoutT Identity

deriving instance Eq Payout

deriving instance Read Payout

deriving instance Show Payout

instance Table PayoutT where
  data PrimaryKey PayoutT f = PayoutId (C f UUID)
                              deriving (Generic, Beamable)
  primaryKey Payout {id} = PayoutId id

deriving instance Eq (PrimaryKey PayoutT Identity)

deriving instance Read (PrimaryKey PayoutT Identity)

deriving instance Show (PrimaryKey PayoutT Identity)

data RefundT f = Refund
  { id :: C f UUID
  , delegate :: PrimaryKey DelegateT f
  , size :: C f (FixedQty "XTZ")
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Refund = RefundT Identity

deriving instance Eq Refund

deriving instance Read Refund

deriving instance Show Refund

instance Table RefundT where
  data PrimaryKey RefundT f = RefundId (C f UUID)
                              deriving (Generic, Beamable)
  primaryKey Refund {id} = RefundId id

deriving instance Eq (PrimaryKey RefundT Identity)

deriving instance Read (PrimaryKey RefundT Identity)

deriving instance Show (PrimaryKey RefundT Identity)

data DelegationT f = Delegation
  { delegator :: C f (Text' "Address")
  , delegate :: PrimaryKey DelegateT f
  , rightsCycle :: PrimaryKey CycleT f
  , size :: C f (FixedQty "XTZ")
  , dividend :: C f (FixedQty "XTZ")
  , payout :: PrimaryKey PayoutT f
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Delegation = DelegationT Identity

deriving instance Eq Delegation

deriving instance Read Delegation

deriving instance Show Delegation

instance Table DelegationT where
  data PrimaryKey DelegationT f = DelegationDelegator (C f
                                                       (Text' "Address"))
                                                    (PrimaryKey DelegateT f) (PrimaryKey CycleT f)
                                  deriving (Generic, Beamable)
  primaryKey Delegation {delegator, delegate, rightsCycle} =
    DelegationDelegator delegator delegate rightsCycle

deriving instance Eq (PrimaryKey DelegationT Identity)

deriving instance Read (PrimaryKey DelegationT Identity)

deriving instance Show (PrimaryKey DelegationT Identity)

data ConfirmedTxT f = ConfirmedTx
  { id :: C f UUID
  , hash :: C f (Text' "TxHash")
  , index :: C f Word64
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type ConfirmedTx = ConfirmedTxT Identity

deriving instance Eq ConfirmedTx

deriving instance Read ConfirmedTx

deriving instance Show ConfirmedTx

instance Table ConfirmedTxT where
  data PrimaryKey ConfirmedTxT f = ConfirmedTxHashId (C f UUID)
                                   deriving (Generic, Beamable)
  primaryKey ConfirmedTx {id} = ConfirmedTxHashId id

deriving instance Eq (PrimaryKey ConfirmedTxT Identity)

deriving instance Read (PrimaryKey ConfirmedTxT Identity)

deriving instance Show (PrimaryKey ConfirmedTxT Identity)

data ConfirmedPaymentT f = ConfirmedPayment
  { hash :: C f (Text' "TxHash")
  , size :: C f (FixedQty "XTZ")
  , index :: C f Word64
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type ConfirmedPayment = ConfirmedPaymentT Identity

deriving instance Eq ConfirmedPayment

deriving instance Read ConfirmedPayment

deriving instance Show ConfirmedPayment

instance Table ConfirmedPaymentT where
  data PrimaryKey ConfirmedPaymentT f = ConfirmedPaymentHash (C f
                                                              (Text' "TxHash"))
                                        deriving (Generic, Beamable)
  primaryKey ConfirmedPayment {hash} = ConfirmedPaymentHash hash

deriving instance Eq (PrimaryKey ConfirmedPaymentT Identity)

deriving instance Read (PrimaryKey ConfirmedPaymentT Identity)

deriving instance Show (PrimaryKey ConfirmedPaymentT Identity)

data T f = T
  { cycles :: f (TableEntity CycleT)
  , delegates :: f (TableEntity DelegateT)
  , rewards :: f (TableEntity RewardT)
  , payouts :: f (TableEntity PayoutT)
  , refunds :: f (TableEntity RefundT)
  , delegations :: f (TableEntity DelegationT)
  , confirmedTxs :: f (TableEntity ConfirmedTxT)
  , confirmedPayments :: f (TableEntity ConfirmedPaymentT)
  } deriving (Generic)

schema :: DatabaseSettings be T
schema = defaultDbSettings
