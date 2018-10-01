module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import VestPrelude
import qualified Streamly

-- | This implementation uses callbacks instread of streaming interfaces because
-- streamly streams don't have persistence and TB[M]Queues are ugly.
class RpcTransport t where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
       -- ^ Called on request, supplied with (headers request respond)
    -> Text' "Route"
    -> t
    -> IO ()
    -- ^ Returns a stream of requests, with response function per-request
  _issueRequest ::
       (Text' "Response" -> IO ()) -- ^ Called on response
    -> Text' "Route"
    -> t
    -> Headers
    -> Text' "Request"
    -> IO (IO ())
    -- ^ Returns cleanup function

data Auth a
  = NoAuth
  | Auth a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

class AuthScheme t where
  verify :: Headers -> Text' "Request" -> IO (Maybe (Claims t))

-- Hacky way to match on auth type when we generate TypeScript callers, since we can't use the type
-- family for that purpose. Down the road when the pipeline is clearer, this matching logic could
-- be converted to a typeclass-based solution.
data AuthType
  = NoAuth'
  | TokenAuth'
  deriving (Show)

type family Claims auth = claims | claims -> auth

type instance Claims () = ()

data DirectOrStreaming
  = Direct
  | Streaming
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data Endpoint (t :: DirectOrStreaming) (auth :: Auth *) (route :: k) (req :: *) (res :: *)

data Headers = Headers
  { format :: SerializationFormat
  , token :: Maybe (Text' "AuthToken")
  -- TODO: Signatures and such.
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

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

verifyEmpty :: Headers -> Text' "Request" -> IO (Maybe ())
verifyEmpty _ _ = return (Just ())
