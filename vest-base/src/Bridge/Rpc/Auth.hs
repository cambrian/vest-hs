module Bridge.Rpc.Auth
  ( Verifier(..)
  , Signer(..)
  , Auth(..)
  , AuthClaims
  ) where

import Bridge.Rpc.Prelude (Headers)
import Vest.Prelude

class Verifier a where
  type Claims a
  verifyRequest :: a -> Headers -> Text' "Request" -> Maybe (Claims a)

class Signer a where
  signRequest :: a -> Headers -> Text' "Request" -> Headers

class (Signer (AuthSigner a), Verifier (AuthVerifier a)) =>
      Auth a
  where
  type AuthSigner a
  type AuthVerifier a

type AuthClaims a = Claims (AuthVerifier a)

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'Nothing instead of 'Auth ().
instance Verifier () where
  type Claims () = ()
  verifyRequest () _ _ = Just ()

instance Signer () where
  signRequest () headers _ = headers

instance Auth () where
  type AuthVerifier () = ()
  type AuthSigner () = ()
