module AccessControl.TestClient
  ( module AccessControl.TestClient
  ) where

import qualified AccessControl.Client
import qualified Transport.Amqp as Amqp
import Vest

-- | General purpose test client
data T = T
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

seed :: ByteString
seed = "testSeed"

pubKey :: PublicKey
pubKey = fst $ seedKeyPair seed

instance HasNamespace T where
  namespace = "TestClient"

instance HasRpcTransport Amqp.T T where
  rpcTransport = amqp

-- There has to be a way to automatically derive this... right?
instance AccessControl.Client.Has T where
  accessControlClient = accessControlClient

instance Resource T where
  type ResourceConfig T = Amqp.Config
  make amqpConfig = do
    amqp <- makeLogged amqpConfig
    accessControlClient <-
      AccessControl.Client.make amqp (panic "should be unused") seed
    return $ T {amqp, accessControlClient}
  cleanup = cleanupLogged . amqp
