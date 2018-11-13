module AccessControl.Internal
  ( module AccessControl.Internal
  ) where

import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

type SignedToken = SignedText' "AccessToken"

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
  , publicKey :: PublicKey
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
