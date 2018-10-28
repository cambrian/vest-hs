module Vest.Bridge.Value.Publisher
  ( module Vest.Bridge.Value.Publisher
  ) where

import Vest.Bridge.Value.Prelude
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family ValueNames spec where
  ValueNames () = '[]
  ValueNames (Value_ _ _ _ (name :: Symbol) _) = '[ name]
  ValueNames (a
              :<|> b) = ValueNames a :++ ValueNames b

type family NubValueNames spec where
  NubValueNames () = '[]
  NubValueNames (Value_ _ _ _ (name :: Symbol) _) = '[ name]
  NubValueNames (a
                 :<|> b) = Nub (NubValueNames a :++ NubValueNames b)

type HasUniqueValueNames spec = ValueNames spec ~ NubValueNames spec

type family Values spec where
  Values () = ()
  Values (Value_ _ _ _ _ a) = Stream ValueBuffer a
  Values (a
          :<|> b) = (Values a
                     :<|> Values b)
  -- Would be better to declare like this, but type-level lists cannot be used as argument types:
  -- Values '[] = '[]
  -- Values (Value_ _ _ _ _ a ': values) = ('[ Streamly.Serial a] :++ Values values)
  -- Watch here: https://www.reddit.com/r/haskell/comments/9n0qan/typefamily_monoid/.

class (HasNamespace t) =>
      Publisher t spec
  where
  publish :: t -> Proxy spec -> Values spec -> IO ()

instance HasNamespace t => Publisher t () where
  publish _ _ _ = return ()

instance ( HasUniqueValueNames (a
                                :<|> b)
         , Publisher t a
         , Publisher t b
         ) =>
         Publisher t (a
                      :<|> b) where
  publish t _ (aValues :<|> bValues) = do
    publish t (Proxy :: Proxy a) aValues
    publish t (Proxy :: Proxy b) bValues

instance ( HasNamespace t
         , HasValueTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         , Eq a
         ) =>
         Publisher t (Value_ fmt t transport name a) where
  publish t _ stream =
    void . async $ do
      let valueName = serialize' @'Pretty $ namespaced @t (Proxy :: Proxy name)
          lockId = retag $ valueName <> "/publisher"
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
        const $ do
        send <- publishValue (valueTransport @transport t) valueName
        tapStream_ (send . serialize' @fmt) stream
