import Bridge.Rpc.Prelude (AuthType(..), DirectOrStreamingType(..))
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

main :: IO ()
main = do
  arguments <- getArgs
  wd <- getCurrentDirectory
  (templateFile, outputFile) <-
    case arguments of
      (a:b:_) -> return (a, b)
      _ -> die "no template file provided"
  template <- eitherParseFile $ wd </> templateFile
  either (die . pack) (writeFile (wd </> outputFile) . toStrict) $
    template >>= (`eitherRender` env)
  where
    env =
      fromPairs
        [ "list" .=
          ((map specTsTypesToObject $
            makeSpecTsTypes (Proxy :: Proxy DummyManager.Api)) :: [Object])
        ]
