module AccessControl.Auth
  ( module AccessControl.Auth
  ) where

import qualified AccessControl
import AccessControl.Permissions (IsPermission(..))
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
  , accessToken :: SignedText' "AccessToken"
  }

newtype Verifier permission =
  Verifier PublicKey

instance (IsPermission permission) =>
         RequestVerifier (Verifier permission) where
  type VerifierClaims (Verifier permission) = Claims
  verifyRequest (Verifier accessControlPubKey) headers reqText time = do
    clientSig <- HashMap.lookup signatureHeader headers >>= read @Signature
    accessToken <-
      HashMap.lookup accessTokenHeader headers >>=
      read @(SignedText' "AccessToken")
    AccessControl.AccessToken {publicKey, name, permissions, expiration} <-
      verify' accessControlPubKey accessToken >>=
      read' @AccessControl.AccessToken
    _ <- verify' publicKey (clientSig, reqText)
    if HashSet.member (runtimeRep @permission) permissions && time < expiration
      then Just $ Claims {publicKey, name}
      else Nothing

instance RequestSigner Signer where
  signRequest Signer {secretKey, accessToken} headers reqText =
    let (sig, _) = sign' secretKey reqText
     in HashMap.insert signatureHeader (show sig) $
        HashMap.insert accessTokenHeader (show accessToken) headers

instance (IsPermission permission) => Auth (T permission) where
  type AuthSigner (T permission) = Signer
  type AuthVerifier (T permission) = Verifier permission
