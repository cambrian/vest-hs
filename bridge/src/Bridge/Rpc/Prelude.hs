module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import VestPrelude

class RpcTransport t where
  _serve ::
       ((Text' "Response" -> IO ()) -> Headers -> Text' "Request" -> IO ())
    -- ^ (send response object text to wire -> headers -> request object text)
    -> Text' "Route" -- ^ route to listen for wire-messages on
    -> t -- ^ transport (should be mutated to store cleanup details)
    -> IO ()
  _call ::
       ((Headers -> Text' "Request" -> IO ()) -> Time Second -> Headers -> req -> IO ( Text' "Response" -> IO ()
                                                                                     , IO x
                                                                                     , IO ()))
    -- ^ (add headers and send request object text to wire -> timeout -> request object)
    -- to IO (push response object text, result stream or item, wait-for-done)
    -> Text' "Route" -- ^ route to send wire-message on
    -> t -- ^ transport (should be mutated to store cleanup details)
    -> Time Second -- ^ timeout
    -> Headers
    -> req
    -> IO x

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
