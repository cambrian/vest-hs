-- GHC does not count getTypeScriptDeclarations as a use of a derived TypeScript instance, so the
-- instance definitions are erroneously marked as orphans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Bridge.Prelude (Format)
import Bridge.Rpc.Prelude (Headers, RpcClientException)
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import VestPrelude

-- Symbols will show up in TS as string literals.
instance (KnownSymbol s) => TypeScript (s :: Symbol) where
  getTypeScriptType s = "\"" ++ symbolVal s ++ "\""

-- This is repetitive, but since the splicing happens at compile time and certain types depend on
-- other types having instances of TypeScript, we separate out the derivation splices.
$(deriveTypeScript defaultOptions ''Text')

$(deriveTypeScript defaultOptions ''Format)

$(deriveTypeScript defaultOptions ''Headers)

$(deriveTypeScript defaultOptions ''RpcClientException)

$(deriveTypeScript defaultOptions ''RequestMessage)

$(deriveTypeScript defaultOptions ''ResponseMessage)

main :: IO ()
main =
  putStrLn $
  formatTSDeclarations . concat $
  [ getTypeScriptDeclarations (Proxy :: Proxy Text')
  , getTypeScriptDeclarations (Proxy :: Proxy RpcClientException)
  , getTypeScriptDeclarations (Proxy :: Proxy (Either RpcClientException Text))
  , getTypeScriptDeclarations (Proxy :: Proxy RequestMessage)
  , getTypeScriptDeclarations (Proxy :: Proxy ResponseMessage)
  ]
