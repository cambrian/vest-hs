module Typescript
  ( module Typescript
  ) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.Types
import Data.List (nub)
import qualified DummyManager.Auth as DummyAuth
import GHC.TypeLits
import qualified Transport.WebSocket as WebSocket
import Vest

data SpecTsTypes = SpecTsTypes
  { hasAuth :: Bool
  , isStreaming :: Bool
  , timeoutMillis :: Int
  , route :: Text' "route"
  , req :: Text' "tsReqType"
  , res :: Text' "tsResType"
  } deriving (Show)

toTsTypeText' :: (TypeScript a) => Proxy a -> Text' t
toTsTypeText' = Tagged . pack . getTypeScriptType

-- JS has unexpected semantics for a timeout value of zero, so we clamp timeouts to a millisecond.
toTotalMillis :: Time Second -> Int
toTotalMillis (Time 0) = 1
toTotalMillis timeout =
  2 * toNum @Millisecond @Int (timeoutsPerHeartbeat *:* timeout)
  -- Timeout after 2 heartbeats.

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
    nub
      (generateTsDeclarations (Proxy :: Proxy a) ++
       generateTsDeclarations (Proxy :: Proxy b))
  makeSpecTsTypes ::
       Proxy (a
              :<|> b)
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    makeSpecTsTypes (Proxy :: Proxy a) ++ makeSpecTsTypes (Proxy :: Proxy b)

instance (KnownNat timeout, KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint_ timeout format 'NoAuth service transport (route :: Symbol) req ('Direct res)) where
  generateTsDeclarations ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Direct res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Direct res))
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { hasAuth = False
        , isStreaming = False
        , timeoutMillis = toTotalMillis $ natSeconds @timeout
        , route = symbolText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownNat timeout, KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint_ timeout format 'NoAuth service transport (route :: Symbol) req ('Streaming res)) where
  generateTsDeclarations ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Streaming res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Streaming res))
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { hasAuth = False
        , isStreaming = True
        , timeoutMillis = toTotalMillis $ natSeconds @timeout
        , route = symbolText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownNat timeout, KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint_ timeout format ('Auth DummyAuth.T) service transport (route :: Symbol) req ('Direct res)) where
  generateTsDeclarations ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Direct res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Direct res))
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { hasAuth = True
        , isStreaming = False
        , timeoutMillis = toTotalMillis $ natSeconds @timeout
        , route = symbolText' (Proxy :: Proxy route)
        , req = toTsTypeText' (Proxy :: Proxy req)
        , res = toTsTypeText' (Proxy :: Proxy res)
        }
    ]

instance (KnownNat timeout, KnownSymbol route, TypeScript req, TypeScript res) =>
         Collector (Endpoint_ timeout format ('Auth DummyAuth.T) service transport (route :: Symbol) req ('Streaming res)) where
  generateTsDeclarations ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Streaming res))
    -> [TSDeclaration]
  generateTsDeclarations _ =
    (getTypeScriptDeclarations (Proxy :: Proxy req)) ++
    (getTypeScriptDeclarations (Proxy :: Proxy res))
  makeSpecTsTypes ::
       Proxy (Endpoint_ timeout format auth service transport route req ('Streaming res))
    -> [SpecTsTypes]
  makeSpecTsTypes _ =
    [ SpecTsTypes
        { hasAuth = True
        , isStreaming = True
        , timeoutMillis = toTotalMillis $ natSeconds @timeout
        , route = symbolText' (Proxy :: Proxy route)
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

instance (KnownSymbol (AppendSymbol s "Id")) =>
         TypeScript (UUID' (s :: Symbol)) where
  getTypeScriptType _ =
    getTypeScriptType (Proxy :: Proxy (Text_ (AppendSymbol s "Id")))
  getTypeScriptDeclarations _ =
    getTypeScriptDeclarations (Proxy :: Proxy (Text_ (AppendSymbol s "Id")))

-- This is repetitive, but since the splicing happens at compile time and certain types depend on
-- other types having instances of TypeScript, we separate out the derivation splices. For the same
-- reason, these instance declarations do not sit next to their types.
$(deriveTypeScript defaultOptions ''RpcResponse)

$(deriveTypeScript defaultOptions ''StreamingResponse)

$(deriveTypeScript defaultOptions ''WebSocket.RequestMessage)

$(deriveTypeScript defaultOptions ''WebSocket.ResponseMessage)
