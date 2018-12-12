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

type SignedOperation = Text' "TzSignedOperation"

type OperationObject = Text' "TzSerializedOperationObject"

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
  , time :: Time
  , operations :: [OperationHash]
  , originations :: [Origination]
  , transactions :: [Transaction]
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Indexable BlockEvent where
  type IndexOf BlockEvent = Word64
  index = number

-- | The Word64 field is the number of blocks for which the operation has been in a given state
-- (i.e. confirmations of that state).
-- Eventually: Move back to Vest.Prelude.Blockchain if other blockchains use this?
data OperationStatus
  = NotIncluded Word64
  | Included (BlockHash, Word64)
  | Confirmed BlockHash
  | Rejected
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data InvalidCycleException =
  InvalidCycleException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

defaultFinalizationLag :: Word64
defaultFinalizationLag = 60
