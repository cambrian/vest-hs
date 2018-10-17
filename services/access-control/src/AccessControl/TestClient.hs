module AccessControl.TestClient
  ( module AccessControl.TestClient
  ) where

import qualified AccessControl
import qualified AccessControl.Auth
import qualified Transport.Amqp as Amqp
import Vest

seed :: ByteString
seed = "01234567890123456789012345678901"

data T = T
  { amqp :: Amqp.T
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , accessToken :: AccessControl.Token
  }

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Auth.HasSigner T where
  secretKey = secretKey
  accessToken = accessToken

data Config = Config
  { amqpConfig :: Amqp.Config
  }

instance Resource T where
  type ResourceConfig T = Config
  make Config {amqpConfig} = do
    (publicKey, secretKey) <- seedKeyPairUnsafe seed
    amqp <- make amqpConfig
    let getAccessToken =
          makeClient amqp (Proxy :: Proxy AccessControl.TokenEndpoint)
    accessToken <- getAccessToken publicKey
    return $ T {amqp, publicKey, secretKey, accessToken}
  cleanup T {amqp} = cleanup amqp

-- This instance is not really overlapping but for some reason GHC thinks it is.
instance {-# OVERLAPPING #-} HasNamespace T where
  namespace = moduleName' @T
