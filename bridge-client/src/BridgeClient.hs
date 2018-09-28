-- GHC does not count getTypeScriptDeclarations as a use of a derived TypeScript instance, so the
-- instance definitions are erroneously marked as orphans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BridgeClient
  ( module BridgeClient
  ) where

import Bridge.Rpc.AuthSchemes
import Bridge.Rpc.Prelude
  ( Auth(..)
  , AuthType(..)
  , DirectOrStreaming(..)
  , Endpoint
  , Headers
  , ResultItem
  , RpcClientException
  )
import Bridge.Transports.WebSocket (RequestMessage, ResponseMessage)
import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import VestPrelude

data SpecTsTypes = SpecTsTypes
  { directOrStreaming :: DirectOrStreaming
  , authType :: AuthType
  , route :: Text' "routeType"
  , req :: Text' "reqType"
  , res :: Text' "resType"
  } deriving (Show)

toTsTypeText :: (TypeScript a) => Proxy a -> Text' t
toTsTypeText = Text' . pack . getTypeScriptType

-- Used to iterate over the nested API structure, run a (possibly monadic) function on the proxied
-- types of each endpoint, and optionally collect the results in a list.
class Collector spec where
  generateTsDeclarations :: Proxy spec -> [TSDeclaration]
  makeSpecTsTypes :: Proxy spec -> [SpecTsTypes]

instance (Collector a, Collector b) =>
         Collector (a
                    :<|> b) where
  generateTsDeclarations ::
       Proxy (a
              :<|> b)
    -> [TSDeclaration]
  generateTsDeclarations _ =
    generateTsDeclarations (Proxy :: Proxy a) ++
    generateTsDeclarations (Proxy :: Proxy b)
  makeSpecTsTypes ::
       Proxy (a
              :<|> b)
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    makeSpecTsTypes (Proxy :: Proxy a) ++ makeSpecTsTypes (Proxy :: Proxy b)

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint 'Direct 'NoAuth (route :: Symbol) req res) where
  generateTsDeclarations ::
       Proxy (Endpoint t auth route req res) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes :: Proxy (Endpoint t auth route req res) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreaming = Direct
        , authType = NoAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText (Proxy :: Proxy req)
        , res = toTsTypeText (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint 'Streaming 'NoAuth (route :: Symbol) req res) where
  generateTsDeclarations ::
       Proxy (Endpoint t auth route req res) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes :: Proxy (Endpoint t auth route req res) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreaming = Streaming
        , authType = NoAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText (Proxy :: Proxy req)
        , res = toTsTypeText (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint 'Direct ('Auth TokenAuth) (route :: Symbol) req res) where
  generateTsDeclarations ::
       Proxy (Endpoint t auth route req res) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes :: Proxy (Endpoint t auth route req res) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreaming = Direct
        , authType = TokenAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText (Proxy :: Proxy req)
        , res = toTsTypeText (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint 'Streaming ('Auth TokenAuth) (route :: Symbol) req res) where
  generateTsDeclarations ::
       Proxy (Endpoint t auth route req res) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes :: Proxy (Endpoint t auth route req res) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreaming = Direct
        , authType = TokenAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText (Proxy :: Proxy req)
        , res = toTsTypeText (Proxy :: Proxy res)
        }
    ]

-- Symbols will show up in TS as string literals.
instance (KnownSymbol s) => TypeScript (s :: Symbol) where
  getTypeScriptType s = "'" ++ symbolVal s ++ "'"

newtype Text_ (a :: Symbol) =
  Text_ Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''Text_)

instance TypeScript Text' where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy Text_)
  getTypeScriptDeclarations _ = getTypeScriptDeclarations (Proxy :: Proxy Text_)

instance (KnownSymbol s) => TypeScript (Text' (s :: Symbol)) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy (Text_ s))
  getTypeScriptDeclarations _ =
    getTypeScriptDeclarations (Proxy :: Proxy (Text_ s))

-- This is repetitive, but since the splicing happens at compile time and certain types depend on
-- other types having instances of TypeScript, we separate out the derivation splices. For the same
-- reason
$(deriveTypeScript defaultOptions ''SerializationFormat)

$(deriveTypeScript defaultOptions ''Headers)

$(deriveTypeScript defaultOptions ''RpcClientException)

$(deriveTypeScript defaultOptions ''ResultItem)

$(deriveTypeScript defaultOptions ''RequestMessage)

$(deriveTypeScript defaultOptions ''ResponseMessage)
