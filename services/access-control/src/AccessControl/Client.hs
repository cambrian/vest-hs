module AccessControl.Client
  ( module AccessControl.Client
  ) where

-- Use (import unqualified): import AccessControl.Client
import qualified AccessControl
import qualified AccessControl.Auth as Auth
import qualified AccessControl.Permission as Permission
import qualified Stream
import qualified Transport.Amqp as Amqp
import Vest

-- Contains logic for both signing and verification. Not split up because most services will
-- probably use both functions.
data T = T
  { accessControlPublicKey :: PublicKey
  , publicKey :: PublicKey
  , secretKey :: SecretKey
  , accessTokenVar :: TVar ( UUID' "AccessTokenVersion"
                           , AccessControl.SignedToken)
  , unsubscribe :: IO' "Unsubscribe" ()
  }

data Config = Config
  { accessControlPublicKey :: PublicKey
  , seed :: ByteString
  , amqp :: Amqp.T
  }

instance Resource T where
  type ResourceConfig T = Config
  make Config {accessControlPublicKey, seed, amqp} = do
    (publicKey, secretKey) <- seedKeyPairUnsafe seed
    let getAccessToken =
          makeClient amqp (Proxy :: Proxy AccessControl.TokenEndpoint)
    (unsubscribe, tokenVersions) <-
      subscribe amqp (Proxy :: Proxy AccessControl.TokenVersionTopic)
    accessTokenVar <-
      tvarFromStreamUnsafe $
      Stream.mapM
        (\version -> do
           token <- getAccessToken publicKey
           return (version, token))
        tokenVersions
    return $
      T
        { accessControlPublicKey
        , publicKey
        , secretKey
        , accessTokenVar
        , unsubscribe
        }
  cleanup T {unsubscribe} = untag unsubscribe

-- These instances are not really overlapping but for some reason GHC thinks they are.
instance {-# OVERLAPPING #-} Permission.Is p => HasAuthSigner (Auth.T p) T where
  authSigner T {secretKey, accessTokenVar} =
    Auth.Signer secretKey (readTVar accessTokenVar >>- snd)

instance {-# OVERLAPPING #-} Permission.Is p =>
                             HasAuthVerifier (Auth.T p) T where
  authVerifier T {accessControlPublicKey, accessTokenVar} =
    Auth.Verifier accessControlPublicKey (readTVar accessTokenVar >>- fst)

class Has t where
  accessControlClient :: t -> T

instance Has T where
  accessControlClient = identity

instance (Permission.Is p, Has t) => HasAuthSigner (Auth.T p) t where
  authSigner = authSigner @(Auth.T p) . accessControlClient

instance (Permission.Is p, Has t) => HasAuthVerifier (Auth.T p) t where
  authVerifier = authVerifier @(Auth.T p) . accessControlClient
