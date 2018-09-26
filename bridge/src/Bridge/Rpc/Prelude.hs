module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import Bridge.Prelude
import VestPrelude

class RpcTransport t where
  _serve ::
       ((Id "ResponseText" -> IO ()) -> Headers -> Id "RequestText" -> IO ())
    -- ^ (send response object text to wire -> headers -> request object text)
    -> Id "Rpc" -- ^ route to listen for wire-messages on
    -> t -- ^ transport (should be mutated to store cleanup details)
    -> IO ()
  _call ::
       ((Headers -> Id "RequestText" -> IO ()) -> Time Second -> Headers -> req -> IO ( Id "ResponseText" -> IO ()
                                                                                      , IO x
                                                                                      , IO ()))
    -- ^ (add headers and send request object text to wire -> timeout -> request object)
    -- to IO (push response object text, result stream or item, wait-for-done)
    -> Id "Rpc" -- ^ route to send wire-message on
    -> t -- ^ transport (should be mutated to store cleanup details)
    -> Time Second -- ^ timeout
    -> Headers
    -> req
    -> IO x

data DirectOrStreaming
  = Direct
  | Streaming
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

type family Claims auth = claims | claims -> auth

class AuthScheme t where
  verify :: Headers -> Id "RequestText" -> IO (Maybe (Claims t))

type instance Claims () = ()

data Headers = Headers
  { format :: Format
  , token :: Maybe Text -- JWT.
  -- TODO: Signatures and such.
  } deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

verifyEmpty :: Headers -> Id "RequestText" -> IO (Maybe ())
verifyEmpty _ _ = return (Just ())

data Auth a
  = NoAuth
  | Auth a
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data Endpoint (t :: DirectOrStreaming) (auth :: Auth *) (route :: k) (req :: *) (res :: *)

data ResultItem a
  = Result a
  | EndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall (Id "RequestText")
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)
