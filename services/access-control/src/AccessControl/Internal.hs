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

-- | sad but necessary for the loadable instance
instance Resource ACPublicKey where
  type ResourceConfig ACPublicKey = ACPublicKey
  make = return
  cleanup _ = return ()

instance Loadable ACPublicKey where
  configName = "access-control/public-key"

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

data T = T
  { subjects :: HashMap PublicKey Subject
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , publicKey :: ACPublicKey
  , secretKey :: SecretKey
  , minTokenTime :: Stream ValueBuffer Time
  , bumpMinTokenTime :: IO ()
  } deriving (HasNamespace)

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasValueTransport Amqp.T T where
  valueTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis
