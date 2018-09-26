module Bridge.Rpc.Prelude
  ( module Bridge.Rpc.Prelude
  ) where

import Bridge.Prelude
import VestPrelude

class RpcTransport t where
  _serve ::
       ((Text -> IO ()) -> Headers -> Text -> IO ())
    -- ^ (send response object text to wire -> headers -> request object text)
    -> Id "Rpc"
    -- ^ route to listen for wire-messages on
    -> t
    -- ^ transport (should be mutated to store cleanup details)
    -> IO ()
  _call ::
       ((Text -> IO ()) -> Time Second -> Headers -> req -> IO ( Text -> IO ()
                                                               , IO x
                                                               , IO ()))
    -- ^ (add headers and send request object text to wire -> timeout -> request object)
    -- to IO (push response object text, result stream or item, wait-for-done)
    -> Id "Rpc"
    -- ^ route to send wire-message on
    -> t
    -- ^ transport (should be mutated to store cleanup details)
    -> Time Second
    -- ^ timeout
    -> Headers
    -> req
    -> IO x

data Arity
  = Direct
  | Streaming

data Endpoint (r :: Arity) (h :: Auth) (s :: k) (a :: *) (b :: *)

data ResultItem a
  = Result a
  | EndOfResults
  deriving (Generic, Read, Show, ToJSON, FromJSON)

data RpcClientException
  = BadAuth
  | BadCall Text
  | BadEndOfResults
  deriving (Eq, Ord, Show, Read, Generic, Exception, FromJSON, ToJSON)
