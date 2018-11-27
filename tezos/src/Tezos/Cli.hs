module Tezos.Cli
  ( module Tezos.Cli
  ) where

import Data.ByteString.Lazy.Char8 (lines)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Process.Typed
import Tezos.Prelude
import Vest hiding (decodeUtf8)

data T = T
  { eztzExe :: FilePath -- Can be as short as eztz-simple if it's in your path.
  , tezosNodeUri :: Text
  , addressSecret :: AddressSecret
  , timeoutSeconds :: Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance Loadable T where
  configFile = [relfile|tezos-cli.yaml|]

extractAddress :: T -> IO Address
-- ^ Should throw if addressSecret is incorrect
extractAddress T {eztzExe, tezosNodeUri, addressSecret = Tagged addressSecret} = do
  let node = "--node " <> unpack tezosNodeUri
      secret = "--secret " <> unpack addressSecret
  (exitCode, output, _) <- readProcess (proc eztzExe ["extract", node, secret])
  case exitCode of
    ExitFailure _ -> throw BugException -- TODO: more descriptive exception
    ExitSuccess -> do
      lastLine <-
        toStrict . decodeUtf8 <$>
        (fromJustUnsafe BugException . last $ lines output)
      return $ Tagged lastLine

forgeTransaction ::
     T -> Address -> FixedQty XTZ -> FixedQty XTZ -> IO SignedOperation
forgeTransaction T { eztzExe = _
                   , tezosNodeUri = _
                   , addressSecret = Tagged _addressSecret
                   , timeoutSeconds = _
                   } (Tagged _recipient) _size _fee = panic "TODO"

-- | TODO: remove once forge/inject are implemented
makePayout :: T -> Address -> FixedQty XTZ -> FixedQty XTZ -> IO OperationHash
makePayout T { eztzExe
             , tezosNodeUri
             , addressSecret = Tagged addressSecret
             , timeoutSeconds
             } (Tagged recipient) size fee = do
  let node = "--node " <> unpack tezosNodeUri
      from = "--fromSK " <> unpack addressSecret
      to = "--toPKH " <> unpack recipient
      amount = "--amount " <> unpack (show @Integer $ fromIntegral size)
      feeRaw = "--fee " <> unpack (show @Integer $ fromIntegral fee)
      timeout = "--timeout " <> unpack (show timeoutSeconds)
  -- ^ Sadly I could not find a Haskell library to auto-format options.
  (exitCode, output, _) <-
    readProcess
      (proc eztzExe ["transfer", node, from, to, amount, feeRaw, timeout])
  case exitCode of
    ExitFailure _ -> panic "TODO: change me"
    ExitSuccess -> do
      lastLine <-
        toStrict . decodeUtf8 <$>
        (fromJustUnsafe BugException . last $ lines output)
      return $ Tagged lastLine
