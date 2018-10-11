module Vest.Bridge.Rpc.Auth
  ( RequestVerifier(..)
  , RequestSigner(..)
  , Auth(..)
  , AuthClaims
  ) where

import Vest.Bridge.Rpc.Prelude (Headers)
import Vest.Prelude

class RequestSigner a where
  signRequest :: a -> Headers -> Text' "Request" -> IO Headers
  -- ^ signing is an IO operation because it may require randomness

class RequestVerifier a where
  type VerifierClaims a
  verifyRequest ::
       a -> Headers -> Text' "Request" -> IO (Maybe (VerifierClaims a))
  -- ^ verifying is an IO operation because it may need to check for expired credentials

class (RequestSigner (AuthSigner a), RequestVerifier (AuthVerifier a)) =>
      Auth a
  where
  type AuthSigner a
  type AuthVerifier a

type AuthClaims a = VerifierClaims (AuthVerifier a)

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'Nothing instead of 'Auth ().
instance RequestSigner () where
  signRequest () headers _ = return headers

instance RequestVerifier () where
  type VerifierClaims () = ()
  verifyRequest () _ _ = return $ Just ()

instance Auth () where
  type AuthSigner () = ()
  type AuthVerifier () = ()
