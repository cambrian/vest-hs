module AccessControl.Auth
  ( T
  , Claims(..)
  , Signer(..)
  , Verifier(..)
  , HasSigner(..)
  , HasVerifier(..)
  ) where

import qualified AccessControl
import qualified AccessControl.Permission as Permission
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

signatureHeader :: Text' "Header"
signatureHeader = Tagged "Signature"

accessTokenHeader :: Text' "Header"
accessTokenHeader = Tagged "AccessToken"

data T permission

data Claims = Claims
  { publicKey :: PublicKey
  , name :: Text
  }

data Signer = Signer
  { secretKey :: SecretKey
  , accessToken :: AccessControl.Token
  }

-- | The wrapped PublicKey should be the AccessControl service's publicKey
newtype Verifier permission =
  Verifier PublicKey

instance (Permission.Is p) => RequestVerifier (Verifier p) where
  type VerifierClaims (Verifier p) = Claims
  verifyRequest (Verifier accessControlPubKey) headers reqText time = do
    clientSig <- HashMap.lookup signatureHeader headers >>= read @Signature
    accessToken <-
      HashMap.lookup accessTokenHeader headers >>= read @AccessControl.Token
    AccessControl.Access {publicKey, name, permissions, expiration} <-
      verify' accessControlPubKey accessToken >>= read' @AccessControl.Access
    _ <- verify' publicKey (clientSig, reqText)
    if HashSet.member (Permission.runtimeRep @p) permissions &&
       time < expiration
      then Just $ Claims {publicKey, name}
      else Nothing

instance RequestSigner Signer where
  signRequest Signer {secretKey, accessToken} headers reqText =
    let (sig, _) = sign' secretKey reqText
     in HashMap.insert signatureHeader (show sig) $
        HashMap.insert accessTokenHeader (show accessToken) headers

instance (Permission.Is p) => Auth (T p) where
  type AuthSigner (T p) = Signer
  type AuthVerifier (T p) = Verifier p

-- | Convenience classes
class HasSigner t where
  secretKey :: t -> SecretKey
  accessToken :: t -> AccessControl.Token

class HasVerifier t where
  accessControlPublicKey :: t -> PublicKey

instance (Permission.Is p, HasSigner t) => HasAuthSigner (T p) t where
  authSigner = Signer <$> secretKey <*> accessToken

instance (Permission.Is p, HasVerifier t) => HasAuthVerifier (T p) t where
  authVerifier = Verifier . accessControlPublicKey
