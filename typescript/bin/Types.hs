import Bridge.Rpc.Prelude (Headers, ResultItem, RpcClientException)
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import BridgeClient
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types ()
import qualified DummyManager
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

data Args = Args
  {
  } deriving (Data, Show)

argsConfig :: Args
argsConfig =
  Args {} &= help "Generates TypeScript types for the dummy-manager API." &=
  summary "ts-types v0.1.0"

main :: IO ()
main = do
  _ <- cmdArgs argsConfig
  versionText <- version
  putText . pack . replaceWithList replaceRules $
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
        , generateTsDeclarations (Proxy :: Proxy DummyManager.Api)
        ]) <>
       "\n")
