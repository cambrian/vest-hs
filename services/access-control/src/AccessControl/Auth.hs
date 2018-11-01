module AccessControl.Auth
  ( T
  , Claims(..)
  , Signer(..)
  , Verifier(..)
  ) where

import qualified AccessControl.Internal as AccessControl
import qualified AccessControl.Permission as Permission
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Vest

signatureHeader :: Text' "Header"
signatureHeader = Tagged "Signature"

tokenHeader :: Text' "Header"
tokenHeader = Tagged "AccessControlSignedToken"

data T permission

data Claims = Claims
  { publicKey :: PublicKey
  , name :: Text
  }

data Signer = Signer
  { secretKey :: SecretKey
  , readSignedToken :: STM AccessControl.SignedToken
  }

data Verifier permission = Verifier
  { accessControlPublicKey :: PublicKey
  , readMinTokenTime :: STM Timestamp
  }

instance (Permission.Is p) => RequestVerifier (Verifier p) where
  type VerifierClaims (Verifier p) = Claims
  verifyRequest Verifier {accessControlPublicKey, readMinTokenTime} headers reqText = do
    minTokenTime <- readMinTokenTime
    return . eitherFromMaybe AuthException $ do
      clientSig <- HashMap.lookup signatureHeader headers >>= read @Signature
      signedToken <-
        HashMap.lookup tokenHeader headers >>= read @AccessControl.SignedToken
      AccessControl.Token {publicKey, name, permissions, time} <-
        verify' accessControlPublicKey signedToken >>=
        read' @AccessControl.Token
      _ <- verify' publicKey (clientSig, reqText)
      if HashSet.member (Permission.runtimeRep @p) permissions &&
         time >= minTokenTime
        then Just $ Claims {publicKey, name}
        else Nothing

instance RequestSigner Signer where
  signRequest Signer {secretKey, readSignedToken} headers reqText = do
    signedToken <- readSignedToken
    let (sig, _) = sign' secretKey reqText
    return $
      HashMap.insert signatureHeader (show sig) $
      HashMap.insert tokenHeader (show signedToken) headers

instance (Permission.Is p) => Auth (T p) where
  type AuthSigner (T p) = Signer
  type AuthVerifier (T p) = Verifier p

-- Could also add a signer instance, which would require creating an access token for
-- AccessControl.T
instance Permission.Is p => HasAuthVerifier (T p) AccessControl.T where
  authVerifier AccessControl.T {publicKey, minTokenTime} =
    Verifier publicKey (justSTM $ readLatestValueSTM minTokenTime)
