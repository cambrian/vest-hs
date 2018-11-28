module AccessControl.TestClient
  ( module AccessControl.TestClient
  ) where

import qualified AccessControl.Client
import qualified Amqp
import Vest

-- | General purpose test client
data T = T
  { amqp :: Amqp.T
  , accessControlClient :: AccessControl.Client.T
  }

seed :: Seed
seed = "testSeed"

pubKey :: PublicKey
pubKey = fst $ seedKeyPair seed

instance HasNamespace T where
  type Namespace T = "TestClient"

instance Has Amqp.T T where
  get = amqp

-- There has to be a way to automatically derive this... right?
instance Has AccessControl.Client.T T where
  get = accessControlClient

make :: Amqp.T -> IO T
make amqp = do
  accessControlClient <-
    AccessControl.Client.make amqp (panic "should be unused") seed
  return $ T {amqp, accessControlClient}
