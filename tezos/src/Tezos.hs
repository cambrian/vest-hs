module Tezos
  ( module Tezos
  ) where

import Data.Conduit ((.|), runConduit)
import Data.Conduit.Combinators (iterM, sinkNull)
import Data.Default.Class as Class
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import qualified Streamly
import qualified Streamly.Prelude as Streamly
import Vest

data PushException =
  PushException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

monitorBlocks :: IO (Streamly.Serial ByteString)
monitorBlocks = do
  (push, _, stream) <- pushStream
  let uri = (http "127.0.0.1" /: "monitor" /: "heads" /: "main")
  async $
    runReq Class.def $
    reqBr GET uri NoReqBody (port 18731) $ \r ->
      runConduit $ responseBodySource r .| iterM push .| sinkNull
  return stream

printBlocks :: IO ()
printBlocks = do
  monitorStream <- monitorBlocks
  Streamly.mapM_ putStrLn monitorStream
