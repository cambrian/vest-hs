module Vest.Bridge.Rpc.Auth
  ( Auth(..)
  , AuthClaims
  , AuthException(..)
  , RequestVerifier(..)
  , RequestSigner(..)
  , HasAuthSigner(..)
  , HasAuthVerifier(..)
  ) where

import Vest.Bridge.Rpc.Prelude (Headers)
import Vest.Prelude

data AuthException =
  AuthException
  deriving (Eq, Read, Show, Generic, Exception, ToJSON, FromJSON)

class RequestSigner a where
  signRequest :: a -> Headers -> Text' "Request" -> IO Headers
  -- Signing is an IO operation because a signer may want to encapsulate logic for renewing expired
  -- credentials, for example. However, you should avoid slow operations because it is called
  -- per-request.

class RequestVerifier a where
  type VerifierClaims a
  verifyRequest ::
       a
    -> Headers
    -> Text' "Request"
    -> IO (Either AuthException (VerifierClaims a))
  -- ^ Verification is an IO operation because it might want to read the current time, or a list of
  -- banned public keys, etc. However, you should avoid slow operations because it is called
  -- per-request.

class (RequestSigner (AuthSigner a), RequestVerifier (AuthVerifier a)) =>
      Auth a
  where
  type AuthSigner a
  type AuthVerifier a

type AuthClaims a = VerifierClaims (AuthVerifier a)

class (Auth a) =>
      HasAuthSigner a t
  where
  authSigner :: t -> AuthSigner a

class (Auth a) =>
      HasAuthVerifier a t
  where
  authVerifier :: t -> AuthVerifier a

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'Nothing instead of 'Auth ().
-- TODO: replace () with data EmptyAuth?
instance RequestSigner () where
  signRequest () headers _ = return headers

instance RequestVerifier () where
  type VerifierClaims () = ()
  verifyRequest () _ _ = return $ Right ()

instance Auth () where
  type AuthSigner () = ()
  type AuthVerifier () = ()

instance HasAuthSigner () t where
  authSigner _ = ()

instance HasAuthVerifier () t where
  authVerifier _ = ()
