module Tezos.Cli
  ( T(..)
  , extractAddress
  , forgeBatchTransaction
  , injectOperation
  ) where

import Control.Retry
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (lines, words)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified GHC.Base
import System.Process.Typed
import Tezos.Prelude
import Vest hiding (decodeUtf8)

data CliRecoverableException =
  CliRecoverableException Text
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

data CliFatalException =
  CliFatalException
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

cliRecoveryCases :: [RetryStatus -> Handler IO Bool]
cliRecoveryCases =
  [ logRetries
      (\(_ :: CliRecoverableException) -> return True)
      (\b e r -> log Debug "recoverable CLI exception" $ defaultLogMsg b e r)
  ]

-- TODO: Remove duplication of this.
milliMicros :: Int
milliMicros = 1000

secMicros :: Int
secMicros = 1000 * milliMicros

cliRetryPolicy :: RetryPolicy
cliRetryPolicy =
  capDelay (30 * secMicros) $ exponentialBackoff (50 * milliMicros)

runCli ::
     FilePath
  -> GHC.Base.String
  -> [[GHC.Base.String]]
  -> (Lazy.ByteString -> IO a)
  -> Bool
  -> IO a
runCli executable command arguments handler recover =
  recovering
    cliRetryPolicy
    cliRecoveryCases
    (const $ do
       (exitCode, output, _) <-
         readProcess (proc executable (command : concat arguments))
       case exitCode of
         ExitFailure _ -> do
           error <- lastWordUnsafe output
           if (error /= "fatal") && recover
             then throw $ CliRecoverableException error
             else log Error (pack $ "CLI " <> command <> " failed") output >>
                  throw CliFatalException
         ExitSuccess -> handler output)

makeOption :: Text -> Text -> [GHC.Base.String]
makeOption flag value = ["--" <> unpack flag, unpack value]

lastWordUnsafe :: Lazy.ByteString -> IO Text
lastWordUnsafe output =
  toStrict . decodeUtf8 <$>
  (fromJustUnsafe CliFatalException . last $ words output)

-- Consider generalizing this scheme.
lastTwoLinesUnsafe :: Lazy.ByteString -> IO (Text, Text)
lastTwoLinesUnsafe output = do
  let lastTwo = take 2 . reverse $ lines output
  case lastTwo of
    [x, y] -> return (toStrict . decodeUtf8 $ x, toStrict . decodeUtf8 $ y)
    _ -> throw CliFatalException

lastLineUnsafe :: Lazy.ByteString -> IO Text
lastLineUnsafe output =
  toStrict . decodeUtf8 <$>
  (fromJustUnsafe CliFatalException . last $ lines output)

-- | Does not recover in any case.
extractAddress :: T -> IO Address
extractAddress T {eztzExe, tezosNodeUri, addressSecret = addressSecretMaybe} = do
  Tagged addressSecret <- fromJustUnsafe MissingKeyException addressSecretMaybe
  let node = makeOption "node" tezosNodeUri
      secret = makeOption "secret" addressSecret
  runCli eztzExe "extract" [node, secret] (fmap Tagged . lastLineUnsafe) False

serializeRecipient :: (Address, FixedQty XTZ) -> Text
serializeRecipient (Tagged address, size) =
  address <> " " <> show @Integer (fromIntegral size)

forgeBatchTransaction ::
     T
  -> HashMap Address (FixedQty XTZ)
  -> FixedQty XTZ
  -> IO (SignedOperation, OperationObject)
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
  runCli
    eztzExe
    "forgeBatchTransfer"
    ([node, from, feeRaw, timeout] ++ recipients)
    (\output -> do
       (signedOpBytes, opObject) <- lastTwoLinesUnsafe output
       return (Tagged signedOpBytes, Tagged opObject))
    True

injectOperation ::
     T -> SignedOperation -> OperationObject -> IO (Maybe OperationHash)
injectOperation T {eztzExe, tezosNodeUri, timeoutSeconds} (Tagged signedOpBytes) (Tagged opObject) = do
  let node = makeOption "node" tezosNodeUri
      signed = makeOption "signed" signedOpBytes
      object = makeOption "object" opObject
      timeout = makeOption "timeout" $ show timeoutSeconds
  catch
    (Just <$>
     runCli
       eztzExe
       "inject"
       [node, signed, object, timeout]
       (fmap Tagged . lastLineUnsafe)
       True)
    (\(_ :: CliFatalException) -> return Nothing)
