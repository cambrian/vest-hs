module AccessControl.Client
  ( module AccessControl.Client
  ) where

-- Use: import qualified AccessControl.Client as AccessControlClient
import qualified AccessControl
import qualified AccessControl.Auth as Auth
import qualified AccessControl.Permission as Permission
import qualified Transport.Amqp as Amqp
import Vest

-- Contains logic for both signing and verification. Not split up because most services will
-- probably use both functions.
data T = T
  { acPublicKey :: AccessControl.ACPublicKey
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , readToken :: STM AccessControl.SignedToken
  , readMinTokenTime :: STM Time
  }

make :: Amqp.T -> AccessControl.ACPublicKey -> Seed -> IO T
make amqp acPublicKey seed = do
  let (publicKey, secretKey) = seedKeyPair seed
      getToken =
        makeClient amqp (Proxy :: Proxy AccessControl.TokenEndpoint) publicKey
  minTokenTimes <-
    subscribe amqp (Proxy :: Proxy AccessControl.TokenVersionValue)
  tokens <- mapMStream (const getToken) minTokenTimes
  void $ streamNext tokens
  return $
    T
      { acPublicKey
      , publicKey
      , secretKey
      , readToken = justSTM $ readLatestValueSTM tokens
      , readMinTokenTime = justSTM $ readLatestValueSTM minTokenTimes
      }

instance Has T t => Has (AuthSigner (Auth.T p)) t where
  get t =
    let authclient = get t
     in Auth.makeSigner (secretKey authclient) (readToken authclient)

instance (Permission.Is p, Has T t) => Has (AuthVerifier (Auth.T p)) t where
  get t =
    let authclient = get t
     in Auth.makeVerifier (acPublicKey authclient) (readMinTokenTime authclient)
