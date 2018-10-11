module Tezos
  ( module Tezos
  ) where

-- import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified GHC.Base
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API hiding (Stream)
import Servant.Client
import qualified Stream
import Tezos.Types
import Vest

data TezosException =
  TezosException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

-- TODO: Un-nest this to make it more composable.
resultLoop ::
     (a -> IO ())
  -> IO' "CloseStream" ()
  -> IO (Maybe (Either GHC.Base.String a))
  -> IO ()
resultLoop push (Tagged close) pull = do
  resultMaybe <- pull
  case resultMaybe of
    Nothing -> close
    Just result ->
      case result of
        Left error -> close >> putText (pack error)
        Right value -> push value >> resultLoop push (Tagged close) pull

monitorBlocks :: IO (Stream Block)
monitorBlocks = do
  (push, Tagged close, stream) <- pushStream
  manager' <- newManager defaultManagerSettings
  async $ do
    either <-
      runClientM
        (client (Proxy :: Proxy MonitorBlocks))
        (mkClientEnv manager' (BaseUrl Http "127.0.0.1" 18731 ""))
    case either of
      Left _ -> close >> putText "request failed"
      Right (ResultStream results) -> results $ resultLoop push (Tagged close)
  return stream

printBlocks :: IO ()
printBlocks = do
  monitorStream <- monitorBlocks
  Stream.mapM_ print monitorStream
