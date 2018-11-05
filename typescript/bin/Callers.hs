import Data.Aeson.Types (Object)
import Data.Text.Lazy (toStrict)
import qualified DummyManager
import System.Directory
import System.FilePath
import Text.EDE
import qualified TezosOperationQueue
import qualified TezosStats
import Typescript
import Vest

callersToGenerate :: [Object]
callersToGenerate =
  [ specToNamespaceObject
      (Proxy :: Proxy DummyManager.T, Proxy :: Proxy DummyManager.Api)
  , specToNamespaceObject
      ( Proxy :: Proxy TezosOperationQueue.T
      , Proxy :: Proxy TezosOperationQueue.Api)
  , specToNamespaceObject
      (Proxy :: Proxy TezosStats.T, Proxy :: Proxy TezosStats.Api)
  -- NOTE: Put any additional APIs here.
  ]

specTsTypesToObject ::
     forall service. (Service service)
  => Proxy service
  -> SpecTsTypes
  -> Object
specTsTypesToObject _ SpecTsTypes { hasAuth
                                  , isStreaming
                                  , timeoutMillis
                                  , route
                                  , req
                                  , res
                                  } =
  fromPairs
    [ "auth" .= hasAuth
    , "streaming" .= isStreaming
    , "timeoutMillis" .= timeoutMillis
    , "route" .= route
    , "namespacedRoute" .= serialize' @'Pretty (namespaced @service route)
    , "req" .= req
    , "res" .= res
    ]

specToNamespaceObject ::
     forall service spec. (Service service, Collector spec)
  => (Proxy service, Proxy spec)
  -> Object
specToNamespaceObject (proxyService, proxySpec) =
  fromPairs
    [ "namespace" .= serviceName @service
    , "specs" .=
      map (specTsTypesToObject proxyService) (makeSpecTsTypes proxySpec)
    ]

data Args = Args
  { templateFile :: Text
  } deriving (Data, Show)

argsConfig :: Args
argsConfig =
  Args {templateFile = "" &= typ "TEMPLATE" &= argPos 0} &=
  help "Generates TypeScript callers for the dummy-manager API." &=
  summary "ts-callers v0.1.0" &=
  program "ts-callers"

main :: IO ()
main = do
  parsedArgs <- cmdArgs argsConfig
  let Args {templateFile} = parsedArgs
  wd <- getCurrentDirectory
  template <- eitherParseFile $ wd </> unpack templateFile
  either (die . pack) (putText . toStrict) $
    template >>= (`eitherRender` fromPairs ["services" .= callersToGenerate])
