module AccessControl
  ( module AccessControl
  ) where

import qualified AccessControl.Permission as Permission
import qualified Data.Yaml as Yaml
import qualified Transports.Amqp as Amqp
import Vest

data Subject = Subject
  { name :: Text
  , permissions :: HashSet Permission.T
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data AccessControl = Args
  { subjectsFile :: FilePath
  , seedFile :: FilePath -- Seed must be exactly 32 bytes.
  , tokenTTLHours :: Int
  } deriving (Data)

data T = T
  { subjects :: HashMap PublicKey Subject
  , amqp :: Amqp.T
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , tokenTTL :: Time Hour
  }

type Token = SignedText' "AccessToken"

data Access = Access
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission.T
  , expiration :: Timestamp
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

type PublicKeyEndpoint
   = Endpoint 'NoAuth T Amqp.T "publicKey" () ('Direct PublicKey)

type TokenEndpoint = Endpoint 'NoAuth T Amqp.T "token" PublicKey ('Direct Token)

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance Service T where
  type ServiceArgs T = AccessControl
  type RpcSpec T = PublicKeyEndpoint
                   :<|> TokenEndpoint
  type PubSubSpec T = ()
  defaultArgs =
    Args
      { subjectsFile = "subjects.yaml"
      , seedFile = "seed.yaml"
      , tokenTTLHours = 1
      }
  init Args {subjectsFile, seedFile, tokenTTLHours} f = do
    (subjects :: HashMap PublicKey Subject) <- Yaml.decodeFileThrow subjectsFile
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    (publicKey, secretKey) <- seedKeyPairUnsafe seed
    let tokenTTL = hour $ fromIntegral tokenTTLHours
    with
      Amqp.localConfig
      (\amqp -> f $ T {subjects, amqp, publicKey, secretKey, tokenTTL})
