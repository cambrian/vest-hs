module Tezos.Cli
  ( module Tezos.Cli
  ) where

import Data.ByteString.Lazy.Char8 (lines)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Process.Typed
import Tezos.Prelude
import Vest hiding (decodeUtf8)

data T = T
  { cliPath :: FilePath -- Expected to work without any further qualification.
  , qualifiedNodeUri :: Text
  , addressSecret :: AddressSecret
  , timeoutInSeconds :: Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance Loadable T where
  configFile = [relfile|tezos-cli.yaml|]

-- | Returns Nothing if the payout was not successfully injected.
makePayout ::
     T -> Address -> FixedQty XTZ -> FixedQty XTZ -> IO (Maybe OperationHash)
makePayout T { cliPath
             , qualifiedNodeUri
             , addressSecret = Tagged addressSecret
             , timeoutInSeconds
             } (Tagged recipient) size fee = do
  let node = "-n " <> unpack qualifiedNodeUri
      from = "-f " <> unpack addressSecret
      to = "-t " <> unpack recipient
      amount = "-a " <> unpack (show @Integer $ fromIntegral size)
      feeRaw = "-p " <> unpack (show @Integer $ fromIntegral fee)
      timeout = "m " <> unpack (show timeoutInSeconds)
  -- ^ Sadly I could not find a Haskell library to auto-format options.
  (exitCode, output, _) <-
    readProcess (proc cliPath [node, from, to, amount, feeRaw, timeout])
  case exitCode of
    ExitFailure _ -> return Nothing
    ExitSuccess -> do
      lastLine <-
        toStrict . decodeUtf8 <$>
        (fromJustUnsafe BugException . last $ lines output)
      return . Just $ Tagged lastLine
