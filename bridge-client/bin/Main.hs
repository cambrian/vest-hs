import Bridge.Transports.WebSocket (RequestMessage)
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import VestPrelude

newtype Test =
  Test Text

$(deriveTypeScript defaultOptions ''Test)

main :: IO ()
main = do
  putStrLn $
    formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy Test))
