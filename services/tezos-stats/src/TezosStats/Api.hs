module TezosStats.Api
  ( module TezosStats.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import qualified Tezos
import qualified TezosStats.Internal as TezosStats
import Vest
import qualified WebSocket

data TimeSize = TimeSize
  { time :: Time
  , size :: Integer' XTZ
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimeSize)

data TimeRate = TimeRate
  { time :: Time
  , rate :: Double
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''TimeRate)

data DelegateFraction = DelegateFraction
  { delegate :: Tezos.ImplicitAddress
  , fraction :: Double
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''DelegateFraction)

data OverviewResponse = OverviewResponse
  { bakerCount :: Int
  , totalRewards :: Integer' XTZ
  , bondsOverTime :: [TimeSize]
  , totalDelegationsOverTime :: [TimeSize]
  , interestRatesOverTime :: [TimeRate]
  , delegateDistribution :: [DelegateFraction]
  , retrieved :: Time
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
  , interestRatesOverTime :: [TimeRate]
  , rewardsOverTime :: [TimeSize]
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''Baker)

data BakersResponse = BakersResponse
  { bakers :: [Baker]
  , retrieved :: Time
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
  , time :: Time
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
  , retrieved :: Time
  } deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''ImplicitResponse)

type ImplicitEndpoint
   = EndpointJson 'NoAuth TezosStats.T WebSocket.T "implicit" Tezos.ImplicitAddress ('Direct ImplicitResponse)

data OperationResponse = OperationResponse
  { baked :: Bool
  , error :: Bool
  , confirmations :: Maybe Int
  , blockHash :: Maybe Tezos.BlockHash
  , retrieved :: Time
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
