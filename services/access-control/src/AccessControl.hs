module AccessControl
  ( module AccessControl
  ) where

import qualified Bridge.Transports.Amqp as Amqp
import qualified Data.Yaml as Yaml
import VestPrelude

type Permission = Text' "Permission"

data Role = Role
  { permissions :: [Permission]
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Subject = Subject
  { publicKey :: Text' "PublicKey"
  , roles :: [Text' "RoleName"]
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Access = Access
  { roles :: HashMap (Text' "RoleName") Role
  , subjects :: HashMap (Text' "SubjectName") Subject
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data T = T
  { amqp :: Amqp.T
  , access :: Access
  }

data AccessControl = Args
  { accessFile :: FilePath
  , keyFile :: FilePath
  } deriving (Data)

type instance ServiceArgs T = AccessControl

instance Service T where
  defaultArgs = Args {accessFile = "access.yaml", keyFile = "key.yaml"}
  run Args {accessFile, keyFile} f = do
    (access :: Access) <- Yaml.decodeFileThrow accessFile
    print access
    with Amqp.localConfig (\amqp -> f $ T {amqp})
