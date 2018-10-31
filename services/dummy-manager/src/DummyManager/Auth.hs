module DummyManager.Auth
  ( T
  ) where

import qualified DummyManager.Internal as DummyManager
import Vest

data T =
  T

instance RequestVerifier T where
  type VerifierClaims T = ()
  verifyRequest _ _ _ = return $ Right ()

instance RequestSigner T where
  signRequest _ headers _ = return headers

instance Auth T where
  type AuthVerifier T = T
  type AuthSigner T = T

instance HasAuthVerifier T DummyManager.T where
  authVerifier _ = T
