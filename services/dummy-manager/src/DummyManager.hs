-- Pointless endpoints for WebSocket testing.
-- TODO: Test a single skipped heartbeat.
module DummyManager
  ( module DummyManager
  ) where

import DummyManager.Api as DummyManager
import DummyManager.Internal as DummyManager
import Vest

addInts :: AddIntsRequest -> IO Int
addInts AddIntsRequest {a, b} = do
  threadDelay (ms 30)
  return (a + b)

echoThrice :: Int -> IO (Stream ValueBuffer Int)
echoThrice x = do
  (writer, stream) <- newStream
  async $
    mapM_ (\n -> threadDelay (ms 30) >> writeStream writer n) [x .. x + 2] >>
    closeStream writer
  return stream

concatTextAuth ::
     AuthClaims () -> ConcatTextAuthRequest -> IO ConcatTextAuthResponse
concatTextAuth _ ConcatTextAuthRequest {a, b} =
  return $ ConcatTextAuthResponse {result = a <> b}

echoThriceAuth :: AuthClaims () -> Text -> IO (Stream ValueBuffer Text)
echoThriceAuth _ = streamFromList . replicate 3

unit :: () -> IO ()
unit _ = return ()

unitStream :: () -> IO (Stream ValueBuffer ())
unitStream _ = streamFromList $ replicate 3 ()

instance Service T where
  type RpcSpec T = AddIntsEndpoint
                   :<|> AddIntsBadEndpoint
                   :<|> EchoThriceEndpoint
                   :<|> EchoThriceBadEndpoint
                   :<|> ConcatTextAuthEndpoint
                   :<|> EchoThriceAuthEndpoint
                   :<|> VoidEndpoint
                   :<|> VoidStreamEndpoint
  type ValueSpec T = ()
  type EventsProduced T = ()
  type EventsConsumed T = ()
  summary = "Dummy Manager v0.1.0"
  description = "Dummy manager service."
  init configPaths f =
    withLoadable configPaths $ \(webSocket :<|> redis) ->
      f $ T {webSocket, redis}
  rpcHandlers _ =
    addInts :<|> addInts :<|> echoThrice :<|> echoThrice :<|> concatTextAuth :<|>
    echoThriceAuth :<|>
    unit :<|>
    unitStream
  masterInstance _ = return ((), (), ())

type PublicApi = RpcSpec T
