module Typescript
  ( module Typescript
  ) where

import Data.Aeson.TypeScript.TH
import Data.List (nub)
import qualified DummyManager.Auth as DummyAuth
import Vest

data SpecTsTypes = SpecTsTypes
  { hasAuth :: Bool
  , isStreaming :: Bool
  , timeoutMillis :: Int
  , route :: Route
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
    getTypeScriptDeclarations (Proxy :: Proxy req) ++
    getTypeScriptDeclarations (Proxy :: Proxy res)
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
    getTypeScriptDeclarations (Proxy :: Proxy req) ++
    getTypeScriptDeclarations (Proxy :: Proxy res)
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
    getTypeScriptDeclarations (Proxy :: Proxy req) ++
    getTypeScriptDeclarations (Proxy :: Proxy res)
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
    getTypeScriptDeclarations (Proxy :: Proxy req) ++
    getTypeScriptDeclarations (Proxy :: Proxy res)
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

-- Used to generate TS declarations from a :<|> list of auxiliary types in an API.
class CollectorRaw spec where
  generateTsAuxiliary :: Proxy spec -> [TSDeclaration]

instance (CollectorRaw a, CollectorRaw b) =>
         CollectorRaw (a
                       :<|> b) where
  generateTsAuxiliary ::
       Proxy (a
              :<|> b)
    -> [TSDeclaration]
  generateTsAuxiliary _ =
    nub
      (generateTsAuxiliary (Proxy :: Proxy a) ++
       generateTsAuxiliary (Proxy :: Proxy b))

instance (TypeScript a) => CollectorRaw (Raw a) where
  generateTsAuxiliary :: Proxy (Raw a) -> [TSDeclaration]
  generateTsAuxiliary _ = getTypeScriptDeclarations (Proxy :: Proxy a)
