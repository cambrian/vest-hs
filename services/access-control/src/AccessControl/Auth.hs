module AccessControl.Auth
  ( T
  , Claims(..)
  , makeSigner
  , makeVerifier
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

instance Auth (T p) where
  type AuthClaims (T p) = Claims

makeSigner :: SecretKey -> STM AccessControl.SignedToken -> AuthSigner (T p)
makeSigner secretKey readSignedToken =
  AuthSigner $ \reqText headers -> do
    signedToken <- readSignedToken
    let (sig, _) = sign' secretKey reqText
    return $
      HashMap.insert signatureHeader (show sig) $
      HashMap.insert tokenHeader (show signedToken) headers

makeVerifier ::
     forall p. Permission.Is p
  => AccessControl.ACPublicKey
  -> STM Time
  -> AuthVerifier (T p)
makeVerifier accessControlPublicKey readMinTokenTime =
  AuthVerifier $ \reqText headers -> do
    minTokenTime <- readMinTokenTime
    return . eitherFromMaybe AuthException $ do
      clientSig <- HashMap.lookup signatureHeader headers >>= read @Signature
      signedToken <-
        HashMap.lookup tokenHeader headers >>= read @AccessControl.SignedToken
      AccessControl.Token {publicKey, name, permissions, time} <-
        verify' (AccessControl.unwrap accessControlPublicKey) signedToken >>=
        read' @AccessControl.Token
      void $ verify' publicKey (clientSig, reqText)
      if HashSet.member (Permission.runtimeRep @p) permissions &&
         time >= minTokenTime
        then Just $ Claims {publicKey, name}
        else Nothing

-- Could also add a signer instance, which would require creating an access token for
-- AccessControl.T
instance Permission.Is p => Has (AuthVerifier (T p)) AccessControl.T where
  get AccessControl.T {publicKey, minTokenTime} =
    makeVerifier publicKey (justSTM $ readLatestValueSTM minTokenTime)
