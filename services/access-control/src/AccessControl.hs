module AccessControl
  ( module AccessControl
  ) where

import qualified Bridge.Transports.Amqp as Amqp
import Data.Aeson
import qualified Data.Yaml as Yaml
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
  , keyFile :: FilePath
  } deriving (Data)

data T = T
  { access :: Access
  , amqp :: Amqp.T
  , keyPair :: KeyPair
  , tokenTTL :: Time Hour
  }

data AccessToken = AccessToken
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission
  , expiration :: Timestamp
  } deriving (Read, Show)

type PubKeyEndpoint
   = Endpoint "Haskell" 'NoAuth T Amqp.T "publicKey" () ('Direct PublicKey)

type AccessTokenEndpoint
   = Endpoint "Haskell" 'NoAuth T Amqp.T "accessToken" PublicKey ('Direct (SignedText' "AccessToken"))

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

-- TODO: move this into Args. Haven't done so yet because Time is not Data-able
defaultTokenTTL :: Time Hour
defaultTokenTTL = hour 24

instance Service T where
  type ServiceArgs T = AccessControl
  type RpcSpec T = PubKeyEndpoint
                   :<|> AccessTokenEndpoint
  type PubSubSpec T = ()
  defaultArgs = Args {accessFile = "access.yaml", keyFile = "key.yaml"}
  init Args {accessFile, keyFile} f = do
    (access :: Access) <- Yaml.decodeFileThrow accessFile
    (keyPair :: KeyPair) <- Yaml.decodeFileThrow keyFile
    print access
    with
      Amqp.localConfig
      (\amqp -> f $ T {access, amqp, keyPair, tokenTTL = defaultTokenTTL})
