module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import Vest.Prelude

-- | This implementation uses callbacks instread of streaming interfaces because Streamly streams
-- don't have persistence and TB[M]Queues are ugly.
class RpcTransport t where
  _consumeRequests ::
       (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -> NamespacedText' "Route"
       -- ^ Called per request, supplied with (headers request respondToClient).
    -> t
    -> IO ()
    -- ^ Returns a stream of requests, with response function per-request.
  _issueRequest ::
       (Text' "Response" -> IO ()) -- ^ Called per response.
    -> NamespacedText' "Route"
    -> t
    -> Headers
    -> Text' "Request"
    -> IO (IO' "Cleanup" ())

-- | Reimplementation of Maybe, but is self-documenting.
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

type Headers = HashMap (Text' "Header") Text

data ResultItem a
  = Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall DeserializeException
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON, Exception)
