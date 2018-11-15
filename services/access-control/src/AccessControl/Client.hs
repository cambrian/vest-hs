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

make :: Amqp.T -> AccessControl.ACPublicKey -> ByteString -> IO T
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

-- These instances overlap with the definitions below when t == T.
-- TODO: There's probably a way to remove the overlap?
instance {-# OVERLAPPING #-} Permission.Is p => HasAuthSigner (Auth.T p) T where
  authSigner T {secretKey, readToken} = Auth.Signer secretKey readToken

instance {-# OVERLAPPING #-} Permission.Is p =>
                             HasAuthVerifier (Auth.T p) T where
  authVerifier T {acPublicKey, readMinTokenTime} =
    Auth.Verifier acPublicKey readMinTokenTime

-- This is a shorthand to allow service implementers to write:
--
-- instance AccessControl.Client.Has T where
--   accessControlClient = accessControlClient
--
-- instead of:
--
-- instance Permission.Is p => HasAuthSigner (AccessControl.Auth.T p) T where
--   authSigner = accessControlClient
--
-- instance Permission.Is p => HasAuthVerifier (AccessControl.Auth.T p) T where
--   authVerifier = accessControlClient
class Has t where
  accessControlClient :: t -> T

instance Has T where
  accessControlClient = identity

instance (Permission.Is p, Has t) => HasAuthSigner (Auth.T p) t where
  authSigner = authSigner @(Auth.T p) . accessControlClient

instance (Permission.Is p, Has t) => HasAuthVerifier (Auth.T p) t where
  authVerifier = authVerifier @(Auth.T p) . accessControlClient
