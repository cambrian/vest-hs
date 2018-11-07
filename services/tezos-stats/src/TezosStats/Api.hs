module TezosStats.Api
  ( module TezosStats.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Tezos
import qualified TezosStats.Internal as TezosStats
import qualified Transport.WebSocket as WebSocket
import Vest

data TimestampSize = TimestampSize
  { timestamp :: UTCTime
  , size :: Integer' XTZ
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimestampSize)

data TimestampRate = TimestampRate
  { timestamp :: UTCTime
  , rate :: Double
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimestampRate)

data DelegateFraction = DelegateFraction
  { delegate :: Tezos.ImplicitAddress
  , fraction :: Double
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''DelegateFraction)

data OverviewResponse = OverviewResponse
  { bakerCount :: Int
  , totalRewards :: Integer' XTZ
  , bondsOverTime :: [TimestampSize]
  , totalDelegationsOverTime :: [TimestampSize]
  , interestRatesOverTime :: [TimestampRate]
  , delegateDistribution :: [DelegateFraction]
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OverviewResponse)

type OverviewEndpoint
   = EndpointJson 'NoAuth TezosStats.T WebSocket.T "overview" () ('Direct OverviewResponse)

data Baker = Baker
  { name :: Text
  , description :: Text
  , hash :: Tezos.ImplicitAddress
  , fee :: Integer' XTZ
  , bond :: Integer' XTZ
  , totalDelegations :: Integer' XTZ
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
   = EndpointJson 'NoAuth TezosStats.T WebSocket.T "bakers" () ('Direct BakersResponse)

data DelegateInfo = DelegateInfo
  { hash :: Tezos.ImplicitAddress
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
  , hash :: Tezos.OperationHash
  , from :: Maybe Tezos.Address -- From account can be either originated or implicit.
  , size :: Integer' XTZ
  , blockHash :: Maybe Tezos.BlockHash
  , timestamp :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''LedgerOperation)

data OriginatedAddress = OriginatedAddress
  { hash :: Tezos.OriginatedAddress
  , delegate :: DelegateInfo
  , balance :: Integer' XTZ
  , ledger :: [LedgerOperation]
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OriginatedAddress)

data ImplicitResponse = ImplicitResponse
  { balance :: Integer' XTZ
  , originated :: [OriginatedAddress]
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''ImplicitResponse)

type ImplicitEndpoint
   = EndpointJson 'NoAuth TezosStats.T WebSocket.T "implicit" Tezos.ImplicitAddress ('Direct ImplicitResponse)

data OperationResponse = OperationResponse
  { baked :: Bool
  , error :: Bool
  , confirmations :: Maybe Int
  , blockHash :: Maybe Tezos.BlockHash
  , retrieved :: UTCTime
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''OperationResponse)

type OperationEndpoint
   = EndpointJson 'NoAuth TezosStats.T WebSocket.T "operation" Tezos.OperationHash ('Streaming OperationResponse)

data StubData = StubData
  { overviewResponse :: OverviewResponse
  , bakersResponse :: BakersResponse
  , implicitResponse :: HashMap Tezos.ImplicitAddress ImplicitResponse
  , operationResponses :: HashMap Tezos.OperationHash [OperationResponse]
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)
