module Tezos.Prelude
  ( module Tezos.Prelude
  ) where

import Vest

type BlockHash = Text' "TzBlockHash"

type OperationHash = Text' "TzOperationHash"

type ImplicitPkh = Text' "TzImplicitPkh"

type OriginatedHash = Text' "TzOriginatedHash"

type AccountHash = Text' "TzAccountHash" -- Either type of account hash.

type SignedOperationContents = Text' "TzSignedOperationContents"

data DelegationInfo = DelegationInfo
  { delegator :: OriginatedHash
  , size :: FixedQty XTZ
  } deriving (Show, Generic, FromJSON)

data RewardInfo = RewardInfo
  { delegate :: ImplicitPkh
  , reward :: FixedQty XTZ
  , staked :: FixedQty XTZ
  , delegations :: [DelegationInfo]
  } deriving (Show, Generic, FromJSON)

data CycleEvent = CycleEvent
  { number :: Word64
  , timestamp :: Timestamp
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable CycleEvent where
  type IndexOf CycleEvent = Word64
  index = number

data OriginationOp = OriginationOp
  { opHash :: OperationHash
  , originator :: ImplicitPkh
  , originated :: OriginatedHash
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data TransactionOp = TransactionOp
  { opHash :: OperationHash
  , from :: AccountHash
  , to :: AccountHash
  , size :: FixedQty XTZ
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Operation
  = Origination OriginationOp
  | Transaction TransactionOp
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
