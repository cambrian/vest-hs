module AccessControl.Internal
  ( module AccessControl.Internal
  ) where

import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

data Subject = Subject
  { name :: Text
  , permissions :: HashSet Permission.T
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

type SignedToken = SignedText' "AccessToken"

data Token = Token
  { publicKey :: PublicKey
  , name :: Text
  , permissions :: HashSet Permission.T
  , version :: UUID' "AccessTokenVersion"
  } deriving (Read, Show, Generic, ToJSON, FromJSON)

data T = T
  { subjects :: HashMap PublicKey Subject
  , amqp :: Amqp.T
  , redis :: RedisConnection
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  -- The redundancy here is a bit sad but it's currently necessary to fit T into the service
  -- interface
  , tokenVersionVar :: TVar (UUID' "AccessTokenVersion")
  , tokenVersions :: Stream (UUID' "AccessTokenVersion")
  , bumpTokenVersion :: IO ()
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

instance HasPubSubTransport Amqp.T T where
  pubSubTransport = amqp

instance HasRedisConnection T where
  redisConnection = redis
