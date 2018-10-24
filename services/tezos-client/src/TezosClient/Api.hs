module TezosClient.Api
  ( module TezosClient.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Vest

data ForgeOriginationRequest = ForgeOriginationRequest
  { manager :: Text' "TzImplicitPkh"
  , delegate :: Text' "TzImplicitPkh"
  , size :: Integer' "OriginationSizeMuTez"
  , fee :: Integer' "OperationFeeMuTez"
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ForgeOriginationRequest)
