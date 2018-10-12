module AccessControl
  ( module AccessControl
  ) where

import Data.Aeson
import qualified Data.Yaml as Yaml
import qualified Transports.Amqp as Amqp
import Vest

import AccessControl.Permissions as AccessControl

data Subject = Subject
  { name :: Text
  , roles :: HashSet (Text' "RoleName")
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Access = Access
  { roles :: HashMap (Text' "RoleName") (HashSet Permission)
  , subjects :: HashMap PublicKey Subject
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data AccessControl = Args
  { accessFile :: FilePath
  , seedFile :: FilePath -- Seed must be exactly 32 bytes.
  } deriving (Data)

data T = T
  { access :: Access
  , amqp :: Amqp.T
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , tokenTTL :: Time Hour
  }

data AccessToken = AccessToken
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission
  , expiration :: Timestamp
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

type PubKeyEndpoint
   = Endpoint "Haskell" 'NoAuth T Amqp.T "publicKey" () ('Direct PublicKey)

type AccessTokenEndpoint
   = Endpoint "Haskell" 'NoAuth T Amqp.T "accessToken" PublicKey ('Direct (SignedText' "AccessToken"))

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

-- TODO: move this into Args. Haven't done so yet because Time is not Data-able
defaultTokenTTL :: Time Hour
defaultTokenTTL = hour 1

data SeedNot32BytesException =
  SeedNot32BytesException
  deriving (Show, Exception)

instance Service T where
  type ServiceArgs T = AccessControl
  type RpcSpec T = PubKeyEndpoint
                   :<|> AccessTokenEndpoint
  type PubSubSpec T = ()
  defaultArgs = Args {accessFile = "access.yaml", seedFile = "seed.yaml"}
  init Args {accessFile, seedFile} f = do
    (access :: Access) <- Yaml.decodeFileThrow accessFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    (publicKey, secretKey) <-
      fromJustUnsafe SeedNot32BytesException $ createKeypairFromSeed_ seed
    print access
    with
      Amqp.localConfig
      (\amqp ->
         f $ T {access, amqp, publicKey, secretKey, tokenTTL = defaultTokenTTL})
