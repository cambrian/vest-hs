module Tezos.Prelude
  ( module Tezos.Prelude
  ) where

import Vest

type BlockHash = Text' "TzBlockHash"

type OperationHash = Text' "TzOperationHash"

type Address = Text' "TzAddress"

-- | The proper way to do this would be to have pure untagging and checked tagging that throws on
-- invalid inputs, but this will do for the time being.
type ImplicitAddress = Tagged "Implicit" Address

type OriginatedAddress = Tagged "Originated" Address

type AddressSecret = Text' "TzAddressSecret"

type SignedOperationContents = Text' "TzSignedOperationContents"

data DelegationInfo = DelegationInfo
  { delegator :: OriginatedAddress
  , size :: FixedQty XTZ
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data RewardInfo = RewardInfo
  { delegate :: ImplicitAddress
  , reward :: FixedQty XTZ
  , stakingBalance :: FixedQty XTZ
  , delegatedBalance :: FixedQty XTZ
  , delegations :: [DelegationInfo]
  } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data CycleEvent = CycleEvent
  { number :: Word64
  , time :: Time
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable CycleEvent where
  type IndexOf CycleEvent = Word64
  index = number

data Origination = Origination
  { hash :: OperationHash
  , originator :: ImplicitAddress
  , originated :: OriginatedAddress
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Transaction = Transaction
  { hash :: OperationHash
  , from :: Address
  , to :: Address
  , fee :: FixedQty XTZ
  , size :: FixedQty XTZ
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data BlockEvent = BlockEvent
  { number :: Word64
  , hash :: BlockHash
  , predecessor :: BlockHash
  , cycleNumber :: Word64
  , fee :: FixedQty XTZ
  , time :: Time
  , operations :: [OperationHash]
  , originations :: [Origination]
  , transactions :: [Transaction]
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable BlockEvent where
  type IndexOf BlockEvent = Word64
  index = number

newtype OperationException =
  OperationException Text
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Exception, ToJSON, FromJSON)

-- | the Word8 field is the number of blocks for which the operation has been in a given state, i.e.
-- confirmations.
-- TODO: move back to Vest.Prelude.Blockchain?
data OperationStatus
  = NotIncluded Word8
  | Included (BlockHash, Word8)
  | Confirmed BlockHash
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)
