module TezosHotWallet
  ( module TezosHotWallet
  ) where

import qualified AccessControl.Client
import qualified Data.Yaml as Yaml
import TezosHotWallet.Api as TezosHotWallet
import TezosHotWallet.Internal as TezosHotWallet
import Vest
import qualified Vest as CmdArgs (name)

data Args = Args
  { configDir :: FilePath
  , seedFile :: FilePath
  } deriving (Data)

defaultArgs_ :: Args
defaultArgs_ =
  Args
    { configDir =
        "services/config/local" &= help "Config directory" &= explicit &=
        CmdArgs.name "config" &=
        CmdArgs.name "c" &=
        typFile
    , seedFile =
        "seed.yaml" &= help "YAML seed file" &= explicit &= CmdArgs.name "seed" &=
        CmdArgs.name "d" &=
        typFile
    } &=
  help "Server to transact Tezos on behalf of Vest." &=
  summary "tezos-hot-wallet v0.1.0" &=
  program "tezos-hot-wallet"

type Api = ()

handlers :: Handlers Api
handlers = ()

instance Service T where
  type ServiceArgs T = Args
  type ValueSpec T = ()
  type EventsProduced T = PaymentEvents
  type EventsConsumed T = ()
  type RpcSpec T = Api
  defaultArgs = defaultArgs_
  init Args {configDir, seedFile} f = do
    (seed :: ByteString) <- Yaml.decodeFileThrow seedFile
    accessControlPublicKey <- load configDir
    withLoadable configDir $ \(dbPool :<|> amqp :<|> redis :<|> tezos) -> do
      accessControlClient <-
        AccessControl.Client.make amqp accessControlPublicKey seed
      f $ T {dbPool, amqp, redis, tezos, accessControlClient}
  rpcHandlers _ = ()
  valuesPublished _ = ()
  eventProducers _ = panic "unimplemented"
  eventConsumers _ = ()
