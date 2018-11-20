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

-- NOTE: Functions still untested.
extractAddress :: T -> IO (Maybe Address)
extractAddress T { cliPath
                 , qualifiedNodeUri
                 , addressSecret = Tagged addressSecret
                 } = do
  let node = "--node " <> unpack qualifiedNodeUri
      secret = "--secret " <> unpack addressSecret
  (exitCode, output, _) <- readProcess (proc cliPath ["extract", node, secret])
  case exitCode of
    ExitFailure _ -> return Nothing
    ExitSuccess -> do
      lastLine <-
        toStrict . decodeUtf8 <$>
        (fromJustUnsafe BugException . last $ lines output)
      return . Just $ Tagged lastLine

-- | Returns Nothing if the payout was not successfully injected.
makePayout ::
     T -> Address -> FixedQty XTZ -> FixedQty XTZ -> IO (Maybe OperationHash)
makePayout T { cliPath
             , qualifiedNodeUri
             , addressSecret = Tagged addressSecret
             , timeoutInSeconds
             } (Tagged recipient) size fee = do
  let node = "--node " <> unpack qualifiedNodeUri
      from = "--fromSK " <> unpack addressSecret
      to = "--toPKH " <> unpack recipient
      amount = "--amount " <> unpack (show @Integer $ fromIntegral size)
      feeRaw = "--fee " <> unpack (show @Integer $ fromIntegral fee)
      timeout = "--timeout " <> unpack (show timeoutInSeconds)
  -- ^ Sadly I could not find a Haskell library to auto-format options.
  (exitCode, output, _) <-
    readProcess
      (proc cliPath ["transfer", node, from, to, amount, feeRaw, timeout])
  case exitCode of
    ExitFailure _ -> return Nothing
    ExitSuccess -> do
      lastLine <-
        toStrict . decodeUtf8 <$>
        (fromJustUnsafe BugException . last $ lines output)
      return . Just $ Tagged lastLine
