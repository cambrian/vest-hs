module AccessControl.Internal
  ( module AccessControl.Internal
  ) where

import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

-- We had to put the core types in their own file because the access control auth / client need to
-- import then.
data Subject = Subject
  { name :: Text
  , permissions :: HashSet Permission.T
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

type SignedToken = SignedText' "AccessToken"

data Token = Token
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission.T
  , time :: Timestamp
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data T = T
  { subjects :: HashMap PublicKey Subject
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  -- The redundancy here is a bit sad, but necessary bc it's not possible to publish a TVar.
  , getMinTokenTime :: STM Timestamp
  , minTokenTimes :: Stream Timestamp
  , bumpMinTokenTime :: IO ()
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasPubSubTransport Amqp.T T where
  pubSubTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis
