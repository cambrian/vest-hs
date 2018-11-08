module Tezos.Prelude
  ( module Tezos.Prelude
  ) where

import Vest

type BlockHash = Text' "TzBlockHash"

type OperationHash = Text' "TzOperationHash"

type ImplicitAccount = Text' "TzImplicitPkh"

type OriginatedAccount = Text' "TzOriginatedHash"

type Account = Text' "TzAccountHash" -- Either type of account hash.

type SignedOperationContents = Text' "TzSignedOperationContents"

data DelegationInfo = DelegationInfo
  { delegator :: OriginatedAccount
  , size :: FixedQty XTZ
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data RewardInfo = RewardInfo
  { delegate :: ImplicitAccount
  , reward :: FixedQty XTZ
  , stakingBalance :: FixedQty XTZ
  , delegatedBalance :: FixedQty XTZ
  , delegations :: [DelegationInfo]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data CycleEvent = CycleEvent
  { number :: Word64
  , timestamp :: Timestamp
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable CycleEvent where
  type IndexOf CycleEvent = Word64
  index = number

data Origination = Origination
  { hash :: OperationHash
  , originator :: ImplicitAccount
  , originated :: OriginatedAccount
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Transaction = Transaction
  { hash :: OperationHash
  , from :: Account
  , to :: Account
  , fee :: FixedQty XTZ
  , size :: FixedQty XTZ
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Operation
  = OriginationOp Origination
  | TransactionOp Transaction
  | Other OperationHash
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data BlockEvent = BlockEvent
  { number :: Word64
  , hash :: BlockHash
  , cycleNumber :: Word64
  , fee :: FixedQty XTZ
  , timestamp :: Timestamp
  , operations :: [Operation]
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable BlockEvent where
  type IndexOf BlockEvent = Word64
  index = number
