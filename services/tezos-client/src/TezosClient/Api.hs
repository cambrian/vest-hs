module TezosClient.Api
  ( module TezosClient.Api
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Tezos.Node
import qualified TezosClient.Internal as TezosClient
import qualified Transport.WebSocket as WebSocket
import Vest

data UnsignedOperations = UnsignedOperations
  { branch :: Text
  , contents :: [Operation]
  } deriving (Eq, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''UnsignedOperations)

data SignedOperations = SignedOperations
  { protocol :: Text
  , branch :: Text
  , contents :: [Operation]
  , signature :: Text
  } deriving (Eq, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data ForgeOriginationsRequest = ForgeOriginationsRequest
  { managerPkh :: Text' "TzImplicitPkh"
  , managerPk :: Text' "TzImplicitPk"
  , delegate :: Text' "TzImplicitPkh"
  , size :: Integer' "OriginationSizeMuTez"
  , fee :: Integer' "OperationFeeMuTez"
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ForgeOriginationsRequest)

data ForgedOperations = ForgedOperations
  { unsignedOperations :: UnsignedOperations
  , operationBytes :: Text' "TzOperationBytes"
  } deriving (Eq, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ForgedOperations)

data ForgeOriginationsResponse = ForgeOriginationsResponse
  { forgedOperations :: ForgedOperations
  , serverSignature :: ByteString' "TzServerSignature"
  } deriving (Eq, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''ForgeOriginationsResponse)

type ForgeOrigination
   = Endpoint_ 5 'JSON 'NoAuth TezosClient.T WebSocket.T "forgeOrigination" ForgeOriginationRequest ('Direct ForgeOriginationResponse)

-- This interface is a little clunky and redundant but works well with EZTZ.
data InjectOriginationRequest = InjectOriginationRequest
  { forgedOperations :: UnsignedOperations
  , serverSignature :: ByteString' "TzServerSignature"
  , clientSignature :: Text' "TzClientSignature" -- Signature on operationBytes.
  , signedOperationBytes :: Text' "TzSignedOperationBytes"
  }

data InjectOriginationResponse = InjectOriginationResponse
  {
  }
