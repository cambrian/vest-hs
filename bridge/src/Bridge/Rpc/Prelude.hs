module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import VestPrelude

-- | This implementation uses callbacks instread of streaming interfaces because
-- streamly streams don't have persistence and TB[M]Queues are ugly.
class RpcTransport t where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> Text' "Route"
       -- ^ Called per request, supplied with (headers request respondToClient)
    -> t
    -> IO ()
    -- ^ Returns a stream of requests, with response function per-request
  _issueRequest ::
       (Text' "Response" -> IO ()) -- ^ Called per response
    -> Text' "Route"
    -> t
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())

data AuthOrNoAuth a
  = NoAuth
  | Auth a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

-- | There's no reason this has to be injective, other than that the compiler complains if it's not
type family AuthClaims auth = claims | claims -> auth

class Auth t where
  verify :: Headers -> Text' "Request" -> IO (Maybe (AuthClaims t))

-- | Streaming endpoints should return cumulative results (missing an intermediate result is ok).
data DirectOrStreaming a
  = Direct a
  | Streaming a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data Endpoint (auth :: AuthOrNoAuth *) (route :: k) (req :: *) (res :: DirectOrStreaming *)

data Headers = Headers
  { format :: SerializationFormat
  , token :: Maybe (Text' "AuthToken")
  -- TODO: Signatures and such.
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

type family Routes spec where
  Routes (Endpoint _ (route :: Symbol) _ _) = '[ route]
  Routes (a
          :<|> b) = Routes a :++ Routes b

type family NubRoutes spec where
  NubRoutes (Endpoint _ (route :: Symbol) _ _) = '[ route]
  NubRoutes (a
             :<|> b) = Nub (NubRoutes a :++ NubRoutes b)

type HasUniqueRoutes spec = Routes spec ~ NubRoutes spec

data ResultItem a
  = Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall (SerializationFormat, Text' "Request")
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON, Exception)

defaultHeaders :: Headers
defaultHeaders = Headers {format = Haskell, token = Nothing}
