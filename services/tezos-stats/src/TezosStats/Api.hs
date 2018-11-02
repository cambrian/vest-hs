module TezosStats.Api
  ( module TezosStats.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified TezosStats.Internal as TezosStats
import qualified Transport.WebSocket as WebSocket
import Vest

data TimestampSize = TimestampSize
  { timestamp :: UTCTime
  , size :: Integer' "XTZ"
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimestampSize)

data TimestampRate = TimestampRate
  { timestamp :: UTCTime
  , rate :: Double
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimestampRate)

data DelegateFraction = DelegateFraction
  { delegate :: Text' "TzImplicitPkh"
  , fraction :: Double
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''DelegateFraction)

data OverviewResponse = OverviewResponse
  { minerCount :: Int
  , totalRewards :: Integer' "XTZ"
  , bondsOverTime :: [TimestampSize]
  , totalDelegationsOverTime :: [TimestampSize]
  , interestRatesOverTime :: [TimestampRate]
  , delegateDistribution :: [DelegateFraction]
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OverviewResponse)

type OverviewEndpoint
   = Endpoint 'NoAuth TezosStats.T WebSocket.T "overview" () ('Direct OverviewResponse)

data Baker = Baker
  { name :: Text
  , description :: Text
  , hash :: Text' "TzImplicitPkh"
  , fee :: Integer' "XTZ"
  , bond :: Integer' "XTZ"
  , totalDelegations :: Integer' "XTZ"
  , overDelegated :: Bool
  , cyclesOutstanding :: Int
  , interestRatesOverTime :: [TimestampRate]
  , rewardsOverTime :: [TimestampSize]
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''Baker)

data BakersResponse = BakersResponse
  { bakers :: [Baker]
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''BakersResponse)

type BakersEndpoint
   = Endpoint 'NoAuth TezosStats.T WebSocket.T "bakers" () ('Direct BakersResponse)

data DelegateInfo = DelegateInfo
  { hash :: Text' "TzImplicitPkh"
  , name :: Text
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''DelegateInfo)

data LedgerOperationType
  = Deposit
  | Reward
  | Withdrawal
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''LedgerOperationType)

data LedgerOperation = LedgerOperation
  { operationType :: LedgerOperationType
  , hash :: Text' "TzOperationHash"
  , from :: Maybe (Text' "TzAccountHash") -- AccountHash can be either originated or implicit.
  , size :: Integer' "XTZ"
  , blockHash :: Maybe (Text' "TzBlockHash")
  , timestamp :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''LedgerOperation)

data OriginatedAccount = OriginatedAccount
  { hash :: Text' "TzOriginatedHash"
  , delegate :: DelegateInfo
  , balance :: Integer' "XTZ"
  , ledger :: [LedgerOperation]
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OriginatedAccount)

data ImplicitResponse = ImplicitResponse
  { balance :: Integer' "XTZ"
  , originated :: [OriginatedAccount]
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''ImplicitResponse)

type ImplicitEndpoint
   = Endpoint 'NoAuth TezosStats.T WebSocket.T "implicit" (Text' "TzImplicitPkh") ('Direct ImplicitResponse)

data OperationResponse = OperationResponse
  { baked :: Bool
  , error :: Bool
  , confirmations :: Maybe Int
  , blockHash :: Maybe (Text' "TzBlockHash")
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OperationResponse)

type OperationEndpoint
   = Endpoint 'NoAuth TezosStats.T WebSocket.T "operation" (Text' "TzOperationHash") ('Streaming OperationResponse)
