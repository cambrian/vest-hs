import Bridge.Transports.WebSocket (RequestMessage)
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Language.Haskell.TH.Syntax
import VestPrelude

-- $(mconcat <$>
--   traverse (deriveTypeScript defaultOptions) [mkName "VestPrelude.Id \"Rpc\""])
$(mconcat <$> traverse (deriveTypeScript defaultOptions) [''Id])

main :: IO ()
main =
  putStrLn $
  formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy Id))
