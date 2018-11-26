module AccessControl.Internal
  ( module AccessControl.Internal
  ) where

import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

type SignedToken = SignedText' "AccessToken"

-- TODO: this is ugly, but replacing PublicKey here makes the implementation of access control
-- itself way uglier
newtype ACPublicKey = ACPublicKey
  { unwrap :: PublicKey
  } deriving newtype (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Loadable ACPublicKey where
  configFile = [relfile|access-control-public-key.yaml|]

data Token = Token
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission.T
  , time :: Time
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data Subject = Subject
  { name :: Text
  , permissions :: HashSet Permission.T
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

instance Loadable (HashMap PublicKey Subject) where
  configFile = [relfile|subjects.yaml|]

data T = T
  { subjects :: HashMap PublicKey Subject
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , publicKey :: ACPublicKey
  , secretKey :: SecretKey
  , minTokenTime :: Stream ValueBuffer Time
  , bumpMinTokenTime :: IO ()
  }

instance HasNamespace T where
  type Namespace T = "access-control"

instance Has Amqp.T T where
  get = amqp

instance Has RedisConnection T where
  get = redis
