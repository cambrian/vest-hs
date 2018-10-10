module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import Vest.Prelude

-- | This implementation uses callbacks instread of streaming interfaces because
-- streamly streams don't have persistence and TB[M]Queues are ugly.
class RpcTransport t where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> NamespacedText' "Route"
       -- ^ Called per request, supplied with (headers request respondToClient)
    -> t
    -> IO ()
    -- ^ Returns a stream of requests, with response function per-request
  _issueRequest ::
       (Text' "Response" -> IO ()) -- ^ Called per response
    -> NamespacedText' "Route"
    -> t
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())

class Verifier a where
  type Claims a
  verify :: a -> Headers -> Text' "Request" -> Maybe (Claims a)

class Signer a where
  sign :: a -> Headers -> Text' "Request" -> Headers

class (Signer (AuthSigner a), Verifier (AuthVerifier a)) =>
      Auth a
  where
  type AuthSigner a
  type AuthVerifier a

type AuthClaims a = Claims (AuthVerifier a)

-- Empty auth instance. This is not intended to be used externally; you should prefer
-- auth 'Nothing instead of 'Auth ().
-- TODO: Add explicit export list and don't export this.
instance Verifier () where
  type Claims () = ()
  verify () _ _ = Just ()

instance Signer () where
  sign () headers _ = headers

instance Auth () where
  type AuthVerifier () = ()
  type AuthSigner () = ()

-- | Reimplementation of Maybe, but is self-documenting
data AuthOrNoAuth a
  = NoAuth
  | Auth a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

-- | Streaming endpoints should return cumulative results (missing an intermediate result is ok).
data DirectOrStreaming a
  = Direct a
  | Streaming a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data Endpoint serializationFormat (auth :: AuthOrNoAuth *) service transport (route :: k) req (res :: DirectOrStreaming *)

class (RpcTransport transport) =>
      HasRpcTransport transport a
  where
  rpcTransport :: a -> transport

data Headers = Headers
  { token :: Maybe (Text' "AuthToken")
  -- TODO: Signatures and such.
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data ResultItem a
  = Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall DeserializeException
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON, Exception)

defaultHeaders :: Headers
defaultHeaders = Headers {token = Nothing}
