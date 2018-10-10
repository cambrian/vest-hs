import BridgeClient
import Data.Aeson.Types (Object)
import Data.Text.Lazy (toStrict)
import qualified DummyManager
import System.Directory
import System.FilePath
import Text.EDE
import VestPrelude

specTsTypesToObject :: SpecTsTypes -> Object
specTsTypesToObject SpecTsTypes { directOrStreamingType
                                , authType
                                , route
                                , req
                                , res
                                } =
  fromPairs
    [ "streaming" .=
      case directOrStreamingType of
        DirectType -> False
        StreamingType -> True
    , "auth" .=
      case authType of
        NoAuth' -> False
        TokenAuth' -> True
    , "route" .= route
    , "req" .= req
    , "res" .= res
    ]

data Args = Args
  { templateFile :: Text
  } deriving (Data, Show)

argsConfig :: Args
argsConfig =
  Args {templateFile = "" &= typ "TEMPLATE" &= argPos 0} &=
  help "Generates TypeScript callers for the dummy-manager API." &=
  summary "ts-callers v0.1.0"

main :: IO ()
main = do
  parsedArgs <- cmdArgs argsConfig
  let Args {templateFile} = parsedArgs
  wd <- getCurrentDirectory
  template <- eitherParseFile $ wd </> unpack templateFile
  either (die . pack) (putText . toStrict) $ template >>= (`eitherRender` env)
  where
    env =
      fromPairs
        [ "list" .=
          ((map specTsTypesToObject $
            makeSpecTsTypes (Proxy :: Proxy DummyManager.Api)) :: [Object])
        ]
