module Vest.Bridge.Rpc.Auth
  ( Auth(..)
  , AuthClaims
  , RequestVerifier(..)
  , RequestSigner(..)
  , HasAuthSigner(..)
  , HasAuthVerifier(..)
  ) where

import Vest.Bridge.Rpc.Prelude (Headers)
import Vest.Prelude

class RequestSigner a where
  signRequest :: a -> Headers -> Text' "Request" -> Headers

class RequestVerifier a where
  type VerifierClaims a
  verifyRequest ::
       a -> Headers -> Text' "Request" -> Timestamp -> Maybe (VerifierClaims a)

class (RequestSigner (AuthSigner a), RequestVerifier (AuthVerifier a)) =>
      Auth a
  where
  type AuthSigner a
  type AuthVerifier a

type AuthClaims a = VerifierClaims (AuthVerifier a)

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'Nothing instead of 'Auth ().
-- TODO: replace () with data EmptyAuth?
instance RequestSigner () where
  signRequest () headers _ = headers

instance RequestVerifier () where
  type VerifierClaims () = ()
  verifyRequest () _ _ _ = Just ()

instance Auth () where
  type AuthSigner () = ()
  type AuthVerifier () = ()

class (Auth a) =>
      HasAuthSigner a t
  where
  authSigner :: t -> AuthSigner a

-- | Empty auth signer is always defined.
instance HasAuthSigner () t where
  authSigner _ = ()

class (Auth a) =>
      HasAuthVerifier a t
  where
  authVerifier :: t -> AuthVerifier a

-- | Empty auth verifier is always defined.
instance HasAuthVerifier () t where
  authVerifier _ = ()
