import Bridge.Rpc.Prelude (RpcClientException)
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import BridgeClient
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types ()
import qualified Manager
import System.Directory
import VestPrelude

main :: IO ()
main = do
  arguments <- getArgs
  wd <- getCurrentDirectory
  file <-
    case head arguments of
      Just _file -> return _file
      Nothing -> die "no output file provided"
  writeFile
    (wd ++ "/" ++ file)
    (pack . formatTSDeclarations . concat $
     [ getTypeScriptDeclarations (Proxy :: Proxy Text')
     , getTypeScriptDeclarations (Proxy :: Proxy RpcClientException)
     , getTypeScriptDeclarations
         (Proxy :: Proxy (Either RpcClientException Text))
     , getTypeScriptDeclarations (Proxy :: Proxy RequestMessage)
     , getTypeScriptDeclarations (Proxy :: Proxy ResponseMessage)
     , generateTsDeclarations (Proxy :: Proxy Manager.ManagerApi)
     ])
