module Vest.Bridge.Rpc.Auth
  ( Auth(..)
  , AuthException(..)
  , AuthSigner(..)
  , AuthVerifier(..)
  ) where

import Vest.Bridge.Rpc.Prelude (Headers)
import Vest.Prelude

data AuthException =
  AuthException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

class Auth a where
  type AuthClaims a

-- | Signing and verification are STM operations because they should be lightweight but
-- nevertheless may want to read state (e.g. the current access token version).
newtype AuthSigner auth = AuthSigner
  { signRequest :: Text' "Request" -> Headers -> STM Headers
  }

newtype AuthVerifier auth = AuthVerifier
  { verifyRequest :: Text' "Request" -> Headers -> STM (Either AuthException (AuthClaims auth))
  }

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'NoAuth instead of 'Auth ().
-- TODO: replace () with data EmptyAuth?
instance Auth () where
  type AuthClaims () = ()

instance Has (AuthSigner ()) a where
  get _ = AuthSigner $ \_ headers -> return headers

instance Has (AuthVerifier ()) a where
  get _ = AuthVerifier $ \_ _ -> return $ Right ()
