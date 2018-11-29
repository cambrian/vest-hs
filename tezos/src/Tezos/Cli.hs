module Tezos.Cli
  ( T(..)
  , extractAddress
  , forgeBatchTransaction
  ) where

import Control.Retry
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (lines)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified GHC.Base
import System.Process.Typed
import Tezos.Prelude
import Vest hiding (decodeUtf8)

data CliRecoverableException =
  CliRecoverableException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

data MissingKeyException =
  MissingKeyException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

data T = T
  { eztzExe :: FilePath -- Can be as short as eztz-simple if it's in your path.
  , tezosNodeUri :: Text
  , addressSecret :: Maybe AddressSecret -- Only necessary for authenticated commands.
  , timeoutSeconds :: Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance Loadable T where
  configFile = [relfile|tezos-cli.yaml|]

makeOption :: Text -> Text -> GHC.Base.String
makeOption flag value = "--" <> unpack flag <> " " <> unpack value

lastLineUnsafe :: Lazy.ByteString -> IO Text
lastLineUnsafe output =
  toStrict . decodeUtf8 <$> (fromJustUnsafe BugException . last $ lines output)

extractAddress :: T -> IO Address
-- ^ Should throw if addressSecret is incorrect.
extractAddress T {eztzExe, tezosNodeUri, addressSecret = addressSecretMaybe} = do
  Tagged addressSecret <- fromJustUnsafe MissingKeyException addressSecretMaybe
  let node = makeOption "node" tezosNodeUri
      secret = makeOption "secret" addressSecret
  (exitCode, output, _) <- readProcess (proc eztzExe ["extract", node, secret])
  case exitCode of
    ExitFailure _ ->
      log Error "address extraction failed" output >> throw BugException
    ExitSuccess -> Tagged <$> lastLineUnsafe output

forgeRecoveryCases :: [RetryStatus -> Handler IO Bool]
forgeRecoveryCases =
  [ logRetries
      (\(_ :: CliRecoverableException) -> return True)
      (\b e r -> log Debug "recoverable CLI exception" $ defaultLogMsg b e r)
  ]

-- TODO: Remove duplication of this.
milliMicros :: Int
milliMicros = 1000

secMicros :: Int
secMicros = 1000 * milliMicros

forgeRetryPolicy :: RetryPolicy
forgeRetryPolicy =
  capDelay (30 * secMicros) $ exponentialBackoff (50 * milliMicros)

serializeRecipient :: (Address, FixedQty XTZ) -> Text
serializeRecipient (Tagged address, size) =
  address <> " " <> show @Integer (fromIntegral size)

forgeBatchTransaction ::
     T -> HashMap Address (FixedQty XTZ) -> FixedQty XTZ -> IO SignedOperation
forgeBatchTransaction T { eztzExe
                        , tezosNodeUri
                        , addressSecret = addressSecretMaybe
                        , timeoutSeconds
                        } recipientMap fee = do
  Tagged addressSecret <- fromJustUnsafe MissingKeyException addressSecretMaybe
  let node = makeOption "node" tezosNodeUri
      from = makeOption "fromSK" addressSecret
      recipients =
        makeOption "recipient" . serializeRecipient <$>
        HashMap.toList recipientMap
      feeRaw = makeOption "fee" $ show @Integer $ fromIntegral fee
      timeout = makeOption "timeout" $ show timeoutSeconds
  -- Retry on probably benign failures (like timeouts).
  recovering
    forgeRetryPolicy
    forgeRecoveryCases
    (const $ do
       (exitCode, output, _) <-
         readProcess
           (proc
              eztzExe
              (["forgeBatchTransfer", node, from, feeRaw, timeout] ++ recipients))
       case exitCode of
         ExitFailure _ -> do
           error <- lastLineUnsafe output
           if error == "Timeout"
             then throw $ CliRecoverableException "timeout"
             else log Error "batch forge failed" output >> throw BugException
         ExitSuccess -> Tagged <$> lastLineUnsafe output)
