module TezosChainWatcher.Db
  ( module TezosChainWatcher.Db
  ) where

import Db
import Vest

data BlockT f = Block
  { hash :: C f (Text' "TzBlockHash")
  , cycle :: C f Word
  , fee :: C f (FixedQty' "TzOperationFee" "XTZ")
  , timestamp :: C f Timestamp
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Block = BlockT Identity

deriving instance Eq Block

deriving instance Read Block

deriving instance Show Block

instance Table BlockT where
  data PrimaryKey BlockT f = BlockHash (C f (Text' "TzBlockHash"))
                             deriving (Generic, Beamable)
  primaryKey Block {hash} = BlockHash hash

deriving instance Eq (PrimaryKey BlockT Identity)

deriving instance Read (PrimaryKey BlockT Identity)

deriving instance Show (PrimaryKey BlockT Identity)

data OriginationT f = Origination
  { opHash :: C f (Text' "TzOperationHash")
  , originator :: C f (Text' "TzOriginatorPkh")
  , originated :: C f (Text' "TzOriginatedPkh")
  , createdAt :: C f Timestamp
  } deriving (Generic, Beamable)

type Origination = OriginationT Identity

deriving instance Eq Origination

deriving instance Read Origination

deriving instance Show Origination

instance Table OriginationT where
  data PrimaryKey OriginationT f = OriginationOpHash (C f
                                                      (Text' "TzOperationHash"))
                                   deriving (Generic, Beamable)
  primaryKey Origination {opHash} = OriginationOpHash opHash

deriving instance Eq (PrimaryKey OriginationT Identity)

deriving instance Read (PrimaryKey OriginationT Identity)

deriving instance Show (PrimaryKey OriginationT Identity)
