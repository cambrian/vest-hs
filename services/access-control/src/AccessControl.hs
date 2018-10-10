module AccessControl
  ( module AccessControl
  ) where

import qualified Bridge.Transports.Amqp as Amqp
import Data.Aeson
import qualified Data.Yaml as Yaml
import qualified ECDSA
import Vest

type Permission = Text' "Permission"

data Role = Role
  { permissions :: [Permission]
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Subject = Subject
  { publicKeyText :: ECDSA.PublicKey
  , roles :: [Text' "RoleName"]
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Access = Access
  { roles :: HashMap (Text' "RoleName") Role
  , subjects :: HashMap (Text' "SubjectName") Subject
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data T = T
  { amqp :: Amqp.T
  , access :: Access
  , keys :: ECDSA.KeyPair
  }

data AccessControl = Args
  { accessFile :: FilePath
  , keyFile :: FilePath
  } deriving (Data)

type PubKeyEndpoint
   = Endpoint "Haskell" 'NoAuth T Amqp.T "publicKey" () ('Direct ECDSA.PublicKey)

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance Service T where
  type ServiceArgs T = AccessControl
  type RpcSpec T = PubKeyEndpoint
  type PubSubSpec T = ()
  defaultArgs = Args {accessFile = "access.yaml", keyFile = "key.yaml"}
  init Args {accessFile, keyFile} f = do
    (access :: Access) <- Yaml.decodeFileThrow accessFile
    print access
    with Amqp.localConfig (\amqp -> f $ T {amqp})
