module Vest.Bridge.Rpc.Prelude
  ( module Vest.Bridge.Rpc.Prelude
  ) where

import qualified Control.Exception as Evil
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Vest.Prelude

type Route = Text' "Route"

newtype AlreadyServingException =
  AlreadyServingException Route
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Exception, Hashable, FromJSON, ToJSON)

newtype InvalidCallException =
  InvalidCallException Text
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Exception, Hashable, FromJSON, ToJSON)

-- | We use callbacks instread of streaming interfaces because Streamly streams
-- don't have persistence and TB[M]Queues are ugly.
class RpcTransport t where
  serveRaw ::
       t
    -> Route
    -> (Headers -> Text' "Request" -> (Text' "Response" -> IO ()) -> IO (Async ()))
    -- ^ Called per request, supplied with (headers request respondFn).
    -> IO ()
  callRaw ::
       t
    -> Route
    -> Headers
    -> Text' "Request"
    -> (IO (Text' "Response") -> IO a)
    -- ^ Response consumer callback. The transport may run cleanup after this fn finishes.
    -> IO a

-- | A descriptive reimplementation of Maybe.
data AuthOrNoAuth a
  = NoAuth
  | Auth a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data DirectOrStreaming a
  = Direct a
  | Streaming a

-- | Base RPC endpoint type.
--
-- Streaming endpoints should return cumulative results.
-- In other words, missing an intermediate result should be ok.
--
-- Streaming endpoints send heartbeats, in addition to results. A heartbeat is sent immediately
-- once a request is validated, and again every so often so that the client knows the server is
-- still responding. The heartbeat interval is currently set from the timeout interval (2 timeouts).
-- A result should be interpreted as an implicit heartbeat.
--
-- The client binding for a streaming endpoint should block until the first heartbeat is received
-- and throw a TimeoutException if nothing is received within the timeout period.
-- It should throw a HeartbeatLostException if it receives nothing for 2 heartbeat intervals.
--
-- We considered setting the heartbeat interval independently from the timeout interval, but
-- ultimately found it cluttery.
data Endpoint_ (timeoutSeconds :: Nat) (fmt :: SerializationFormat) (auth :: AuthOrNoAuth Type) service transport (route :: k) req (res :: DirectOrStreaming Type)

type DefaultTimeoutSeconds = 5

type Endpoint = Endpoint_ DefaultTimeoutSeconds 'Haskell

type EndpointJson = Endpoint_ DefaultTimeoutSeconds 'JSON

timeoutsPerHeartbeat :: Rational
timeoutsPerHeartbeat = 2

type Headers = HashMap (Text' "Header") Text

data RpcResponse a
  = RpcResponseClientException Text
  | RpcResponseServerException
  | RpcResponse a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''RpcResponse)

data StreamingResponse a
  = Heartbeat
  | Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

$(deriveTypeScript defaultOptions ''StreamingResponse)

newtype HeartbeatLostException =
  HeartbeatLostException Duration
  deriving (Eq, Read, Show, Generic)
  deriving anyclass (Evil.Exception, ToJSON, FromJSON)
