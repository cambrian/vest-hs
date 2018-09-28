import Bridge.Rpc.Prelude (Headers, ResultItem, RpcClientException)
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import BridgeClient
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types ()
import Data.Text (unlines)
import qualified Manager
import System.Directory
import System.FilePath
import Text.Replace
import VestPrelude

nominal :: Text
nominal =
  unlines
    [ "class Nominal<T extends string> {"
    , "  // @ts-ignore"
    , "  private as: T"
    , "}\n"
    ]

-- Hard-coded substitutions.
replaceRules :: [Replace]
replaceRules =
  [ Replace
      "type Text_<T> = IText_<T>"
      "type Text_<T extends string> = string & Nominal<T>"
  , Replace "\n\ntype IText_<T> = string" ""
  , Replace "IEndOfResults<T>" "IEndOfResults"
  , Replace "\"" "\'"
  , Replace ";" ""
  ]

main :: IO ()
main = do
  arguments <- getArgs
  wd <- getCurrentDirectory
  file <-
    case head arguments of
      Just _file -> return _file
      Nothing -> die "no output file provided"
  writeFile (wd </> file) $
    pack . replaceWithList replaceRules $
    unpack
      (nominal <>
       (pack . formatTSDeclarations . concat $
        [ getTypeScriptDeclarations (Proxy :: Proxy SerializationFormat)
        , getTypeScriptDeclarations (Proxy :: Proxy Headers)
        , getTypeScriptDeclarations (Proxy :: Proxy Text')
        , getTypeScriptDeclarations (Proxy :: Proxy RpcClientException)
        , getTypeScriptDeclarations
            (Proxy :: Proxy (Either RpcClientException Text))
        , getTypeScriptDeclarations (Proxy :: Proxy ResultItem)
        , getTypeScriptDeclarations (Proxy :: Proxy RequestMessage)
        , getTypeScriptDeclarations (Proxy :: Proxy ResponseMessage)
        , generateTsDeclarations (Proxy :: Proxy Manager.ManagerApi)
        ]) <>
       "\n")
