-- GHC does not count getTypeScriptDeclarations as a use of a derived TypeScript instance, so the
-- instance definitions are erroneously marked as orphans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BridgeClient
  ( module BridgeClient
  ) where

import qualified Bridge.Rpc.Auth.Token as Token
import Bridge.Rpc.Prelude
  ( AuthOrNoAuth(..)
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

-- Hacky way to match on auth type when we generate TypeScript callers, since we can't use the type
-- family for that purpose. Sometime might be worth cleaning up.
data AuthType
  = NoAuth'
  | TokenAuth'
  deriving (Show)

-- See note for AuthType.
data DirectOrStreamingType
  = DirectType
  | StreamingType
  deriving (Show)

data SpecTsTypes = SpecTsTypes
  { directOrStreamingType :: DirectOrStreamingType
  , authType :: AuthType
  , route :: Text' "route"
  , req :: Text' "tsReqType"
  , res :: Text' "tsResType"
  } deriving (Show)

toTsTypeText' :: (TypeScript a) => Proxy a -> Text' t
toTsTypeText' = Tagged . pack . getTypeScriptType

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
         Collector (Endpoint service 'NoAuth (route :: Symbol) req ('Direct res)) where
  generateTsDeclarations ::
       Proxy (Endpoint service auth route req ('Direct res)) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint service auth route req ('Direct res)) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreamingType = DirectType
        , authType = NoAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint service 'NoAuth (route :: Symbol) req ('Streaming res)) where
  generateTsDeclarations ::
       Proxy (Endpoint service auth route req ('Streaming res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint service auth route req ('Streaming res)) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreamingType = StreamingType
        , authType = NoAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint service ('Auth Token.T) (route :: Symbol) req ('Direct res)) where
  generateTsDeclarations ::
       Proxy (Endpoint service auth route req ('Direct res)) -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint service auth route req ('Direct res)) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreamingType = DirectType
        , authType = TokenAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint service ('Auth Token.T) (route :: Symbol) req ('Streaming res)) where
  generateTsDeclarations ::
       Proxy (Endpoint service auth route req ('Streaming res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint service auth route req ('Streaming res)) -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { directOrStreamingType = StreamingType
        , authType = TokenAuth'
        , route = proxyText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

-- Symbols will show up in TS as string literals.
instance (KnownSymbol s) => TypeScript (s :: Symbol) where
  getTypeScriptType s = "\"" ++ symbolVal s ++ "\""

newtype Text_ (a :: Symbol) =
  Text_ Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

$(deriveTypeScript defaultOptions ''Text_)

instance (KnownSymbol s) => TypeScript (Text' (s :: Symbol)) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy (Text_ s))
  getTypeScriptDeclarations _ =
    getTypeScriptDeclarations (Proxy :: Proxy (Text_ s))

-- This COULD be represented in TypeScript but might be more effort than worth.
instance (KnownSymbol s) => TypeScript (NamespacedText' (s :: Symbol)) where
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
