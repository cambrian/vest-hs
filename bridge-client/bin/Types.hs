import Bridge.Rpc.Prelude (Headers, ResultItem, RpcClientException)
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import BridgeClient
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types ()
import qualified Manager
import System.Directory
import System.FilePath
import System.Random
import Text.Replace
import VestPrelude

version :: IO Text
version = do
  versionId <- randomRIO (1, 1000000)
  return $
    "type V" <> show (versionId :: Int) <> " = 'Bridge Typings Version " <>
    show (versionId :: Int) <>
    "'\n\n"

tagged :: Text
tagged = "type Tagged<T extends string, K> = { TagDoNotUse: T } | K\n\n"

-- Hard-coded substitutions.
replaceRules :: [Replace]
replaceRules =
  [ Replace
      "type Text_<T> = IText_<T>"
      "export type Text<T extends string> = Tagged<T, string>"
  , Replace "\n\ntype IText_<T> = string" ""
  , Replace "IEndOfResults<T>" "IEndOfResults"
  , Replace "Text_" "Text"
  , Replace "\"" "\'"
  , Replace ";" ""
  , Replace "type" "export type"
  , Replace "interface" "export interface"
  ]

main :: IO ()
main = do
  arguments <- getArgs
  wd <- getCurrentDirectory
  file <-
    case head arguments of
      Just _file -> return _file
      Nothing -> die "no output file provided"
  versionText <- version
  writeFile (wd </> file) $
    pack . replaceWithList replaceRules $
    unpack
      (versionText <> tagged <>
       (pack . formatTSDeclarations . concat $
        [ getTypeScriptDeclarations (Proxy :: Proxy Text_)
        , getTypeScriptDeclarations (Proxy :: Proxy SerializationFormat)
        , getTypeScriptDeclarations (Proxy :: Proxy Headers)
        , getTypeScriptDeclarations (Proxy :: Proxy RpcClientException)
        , getTypeScriptDeclarations
            (Proxy :: Proxy (Either RpcClientException Text))
        , getTypeScriptDeclarations (Proxy :: Proxy ResultItem)
        , getTypeScriptDeclarations (Proxy :: Proxy RequestMessage)
        , getTypeScriptDeclarations (Proxy :: Proxy ResponseMessage)
        , generateTsDeclarations (Proxy :: Proxy Manager.ManagerApi)
        ]) <>
       "\n")
