module Vest.Bridge.Rpc.Prelude
  ( module Vest.Bridge.Rpc.Prelude
  ) where

import qualified Control.Exception as Evil
import Time.Units (KnownUnitName)
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

-- | Streaming endpoints should return cumulative results.
-- (Missing an intermediate result should be ok.)
data DirectOrStreaming a
  = Direct a
  | Streaming a

-- | Base RPC endpoint type.
-- Streaming endpoints will send a heartbeat immediately once the request is validated, and again
-- at every (timeoutsPerHeartbeat * timeout) interval if the next result has not been produced yet.
-- The client binding for a streaming endpoint should block until the first heartbeat is received,
-- and throw a TimeoutException if it does not arrive within the timeout period.
-- It should also throw a HeartbeatLostException if it misses 2 consecutive heartbeats.
data Endpoint_ (timeoutSeconds :: Nat) serializationFormat (auth :: AuthOrNoAuth *) service transport (route :: k) req (res :: DirectOrStreaming *)

type DefaultTimeoutSeconds = 5

type Endpoint = Endpoint_ DefaultTimeoutSeconds "Haskell"

timeoutsPerHeartbeat :: Ratio Natural
timeoutsPerHeartbeat = 2

class (RpcTransport transport) =>
      HasRpcTransport transport a
  where
  rpcTransport :: a -> transport

type Headers = HashMap (Text' "Header") Text

data StreamingResponse a
  = Heartbeat
  | Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall DeserializeException
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON, Exception)

newtype HeartbeatLostException unit =
  HeartbeatLostException (Time unit)
  deriving (Eq, Ord)

deriving instance
         (KnownUnitName unit) => Show (HeartbeatLostException unit)

instance (Typeable unit, KnownUnitName unit) =>
         Evil.Exception (HeartbeatLostException unit)
