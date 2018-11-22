module Vest.Bridge.Value.Publisher
  ( Values
  , Publisher(..)
  ) where

import Vest.Bridge.Value.Prelude
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family ValueNames spec where
  ValueNames () = '[]
  ValueNames (ValueTopic_ _ _ _ (name :: Symbol) _) = '[ name]
  ValueNames (a
              :<|> b) = ValueNames a :++ ValueNames b

type family NubValueNames spec where
  NubValueNames () = '[]
  NubValueNames (ValueTopic_ _ _ _ (name :: Symbol) _) = '[ name]
  NubValueNames (a
                 :<|> b) = Nub (NubValueNames a :++ NubValueNames b)

type HasUniqueValueNames spec = ValueNames spec ~ NubValueNames spec

type family Values spec where
  Values () = ()
  Values (ValueTopic_ _ _ _ _ a) = Stream ValueBuffer a
  Values (a
          :<|> b) = (Values a
                     :<|> Values b)

class Publisher t spec where
  publish :: t -> Proxy spec -> Values spec -> IO ()

instance Publisher t () where
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
         Publisher t (ValueTopic_ fmt t transport name a) where
  publish t _ stream =
    void . asyncThrows $ do
      let valueName = serialize' @'Pretty $ namespaced @t (Proxy :: Proxy name)
          lockId = retag $ valueName <> "/publisher"
      withDistributedLock t lockId $ do
        send <- publishValue (valueTransport @transport t) valueName
        consumeStream (send . serialize' @fmt) stream
