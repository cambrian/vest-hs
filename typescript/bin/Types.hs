import Data.Aeson.TypeScript.TH
import Data.Aeson.Types ()
import qualified DummyManager
import System.Console.CmdArgs
import System.Random
import Text.Replace
import qualified TezosInjector
import qualified TezosPlatformStats
import Typescript
import Vest hiding (Args, summary)
import qualified WebSocket

typesToGenerate :: [[TSDeclaration]]
typesToGenerate =
  [ generateTsDeclarations (Proxy :: Proxy DummyManager.PublicApi)
  , generateTsDeclarations (Proxy :: Proxy TezosInjector.PublicApi)
  , generateTsDeclarations (Proxy :: Proxy TezosPlatformStats.PublicApi)
  , generateTsDeclarations (Proxy :: Proxy TezosPlatformStats.AuxiliaryTypes)
  -- Put any additional APIs here.
  ]

version :: IO Text
version = do
  versionId <- randomRIO (1, 1000000)
  return $
    "type V" <> show (versionId :: Int) <> " = 'Bridge Typings Version " <>
    show (versionId :: Int) <>
    "'\n\n"

taggedType :: Text
taggedType = "type Tagged<T extends string, K> = { TagDoNotUse: T } | K\n\n"

textAlias :: Text
textAlias = "type Text<T extends string> = Tagged<T, string>\n\n"

unitType :: Text
unitType = "type Unit = Array<Boolean>\n\n"

rpcResponseType :: Text
rpcResponseType = "type RpcResponse<T> = Either<RpcException, T>\n\n"

-- Hard-coded substitutions.
replaceRules :: [Replace]
replaceRules =
  [ Replace "IEndOfResults<T>" "IEndOfResults"
  , Replace "IHeartbeat<T>" "IHeartbeat"
  , Replace "IRpcClientException<T>" "IRpcClientException"
  , Replace "IRpcServerException<T>" "IRpcServerException"
  , Replace "Tagged<\"Header\", string>" "string"
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
  summary "ts-types v0.1.0" &=
  program "ts-types"

main :: IO ()
main = do
  void $ cmdArgs argsConfig
  versionText <- version
  putText . pack . replaceWithList replaceRules $
    unpack
      (versionText <> taggedType <> textAlias <> unitType <> rpcResponseType <>
       (pack . formatTSDeclarations . concat $
        [ getTypeScriptDeclarations (Proxy :: Proxy (RpcResponse ())) -- Either.
        , getTypeScriptDeclarations (Proxy :: Proxy RpcException)
        , getTypeScriptDeclarations (Proxy :: Proxy StreamingResponse)
        , getTypeScriptDeclarations (Proxy :: Proxy WebSocket.RequestMessage)
        , getTypeScriptDeclarations (Proxy :: Proxy WebSocket.ResponseMessage)
        , concat typesToGenerate
        ]))
