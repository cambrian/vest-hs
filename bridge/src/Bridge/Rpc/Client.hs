module Bridge.Rpc.Client
  ( module Bridge.Rpc.Client
  ) where

import Bridge.Prelude
import Bridge.Rpc.Prelude
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import VestPrelude

type family ClientBindings spec where
  ClientBindings (Endpoint 'Direct _ _ req res) = Time Second -> Headers -> req -> IO res
  ClientBindings (Endpoint 'Streaming _ _ req res) = Time Second -> Headers -> req -> IO (Streamly.Serial res)
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

directPusher :: IO (res -> IO (), IO res, IO ())
directPusher = do
  resultVar <- newEmptyMVar
  let push = putMVar resultVar
      result = readMVar resultVar
      waitForDone = void result
  return (push, result, waitForDone)

streamingPusher :: IO (ResultItem res -> IO (), IO (Streamly.Serial res), IO ())
streamingPusher = do
  (push_, close, results) <- pushStream
  let push (Result res) = push_ res
      push EndOfResults = close
      waitForDone = Streamly.mapM_ return results
  return (push, return results, waitForDone)

callProcessor ::
     (Show req, ToJSON req, Read res, FromJSON res)
  => IO (res -> IO (), IO x, IO ())
  -> (Headers -> Id "RequestText" -> IO ())
  -> Time Second
  -> Headers
  -> req
  -> IO (Id "ResponseText" -> IO (), IO x, IO ())
callProcessor pusher send timeout_ headers req = do
  let Headers {format} = headers
  let deserializeUnsafe = deserializeUnsafeOf format
      serialize = serializeOf format
  (push_, result, waitForDone) <- pusher
  (renewTimeout, timeoutDone) <- timeoutRenewable timeout_ waitForDone
  send headers (Id @"RequestText" $ serialize req)
  let push (Id resOrExcText) = do
        resOrExc <- deserializeUnsafe resOrExcText
        case resOrExc of
          Left exc -> throw (exc :: RpcClientException)
          Right res -> push_ res
        renewTimeout
  mainThread <- myThreadId
  async $ do
    timeoutResult <- timeoutDone
    case timeoutResult of
      Nothing -> evilThrowTo mainThread $ TimeoutException timeout_ -- Don't do this at home.
      Just () -> return ()
  return (push, result, waitForDone)

instance ( KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Direct h (route :: Symbol) req res) transport where
  makeClient ::
       Proxy (Endpoint 'Direct h route req res, transport)
    -> transport
    -> (Time Second -> Headers -> req -> IO res)
  makeClient _ =
    _call (callProcessor directPusher) (Id $ proxyText (Proxy :: Proxy route))

instance ( KnownSymbol route
         , Show req
         , ToJSON req
         , Read res
         , FromJSON res
         , RpcTransport transport
         ) =>
         Client (Endpoint 'Streaming h (route :: Symbol) req res) transport where
  makeClient ::
       Proxy (Endpoint 'Streaming h route req res, transport)
    -> transport
    -> (Time Second -> Headers -> req -> IO (Streamly.Serial res))
  makeClient _ =
    _call
      (callProcessor streamingPusher)
      (Id $ proxyText (Proxy :: Proxy route))
