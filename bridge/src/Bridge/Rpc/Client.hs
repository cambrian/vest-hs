module Bridge.Rpc.Client
  ( module Bridge.Rpc.Client
  ) where

import Bridge.Prelude
import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family ClientBindings spec where
  ClientBindings (Endpoint 'Direct _ (s :: Symbol) a b) = Time Second -> Headers -> a -> IO b
  ClientBindings (Endpoint 'Streaming _ (s :: Symbol) a b) = Time Second -> Headers -> a -> IO (Streamly.Serial b)
  ClientBindings (a
                  :<|> b) = (ClientBindings a
                             :<|> ClientBindings b)

class (RpcTransport transport) =>
      Client spec transport
  where
  makeClient :: Proxy (spec, transport) -> transport -> ClientBindings spec

instance (Client a transport, Client b transport) =>
         Client (a
                 :<|> b) transport where
  makeClient ::
       Proxy ( a
               :<|> b
             , transport)
    -> transport
    -> ClientBindings a
       :<|> ClientBindings b
  makeClient _ transport =
    makeClient (Proxy :: Proxy (a, transport)) transport :<|>
    makeClient (Proxy :: Proxy (b, transport)) transport

directPusher :: IO ((ResultItem res) -> IO (), IO res, IO ())
directPusher = do
  resultVar <- newEmptyMVar
  let push (Result res) = putMVar resultVar res
      push EndOfResults = throw BadEndOfResults
      result = readMVar resultVar
      waitForDone = void result
  return (push, result, waitForDone)

streamingPusher ::
     IO ((ResultItem res) -> IO (), IO (Streamly.Serial res), IO ())
streamingPusher = do
  (_push, close, results) <- pushStream
  let push (Result res) = _push res
      push EndOfResults = close
      waitForDone = Streamly.mapM_ return results
  return (push, return results, waitForDone)

callProcessor ::
     (Show req, ToJSON req, Read res, FromJSON res)
  => IO ((ResultItem res) -> IO (), IO x, IO ())
  -> (Text -> IO ())
  -> Time Second
  -> Headers
  -> req
  -> IO (Text -> IO (), IO x, IO ())
callProcessor pusher send _timeout headers req = do
  let Headers {format} = headers
  let deserializeUnsafe = deserializeUnsafeOf format
      serialize = serializeOf format
  (_push, result, waitForDone) <- pusher
  (renewTimeout, timeoutDone) <- timeoutRenewable _timeout waitForDone
  send . serialize $ req
  let push resOrExcText = do
        resOrExc <- deserializeUnsafe resOrExcText
        case resOrExc of
          Left exc -> throw (exc :: RpcClientException)
          Right res -> _push res
        renewTimeout
  mainThread <- myThreadId
  async $ do
    timeoutResult <- timeoutDone
    case timeoutResult of
      Nothing -> evilThrowTo mainThread $ TimeoutException _timeout -- Don't do this at home.
      Just () -> return ()
  return (push, result, waitForDone)

instance ( KnownSymbol s
         , Show a
         , ToJSON a
         , Read b
         , FromJSON b
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Direct (h :: Auth) (s :: Symbol) a b) transport where
  makeClient ::
       Proxy (Endpoint 'Direct h s a b, transport)
    -> transport
    -> (Time Second -> Headers -> a -> IO b)
  makeClient _ =
    _call (callProcessor directPusher) (Id $ proxyText (Proxy :: Proxy s))

instance ( KnownSymbol s
         , Show a
         , ToJSON a
         , Read b
         , FromJSON b
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Streaming (h :: Auth) (s :: Symbol) a b) transport where
  makeClient ::
       Proxy (Endpoint 'Streaming h s a b, transport)
    -> transport
    -> (Time Second -> Headers -> a -> IO (Streamly.Serial b))
  makeClient _ =
    _call (callProcessor streamingPusher) (Id $ proxyText (Proxy :: Proxy s))
