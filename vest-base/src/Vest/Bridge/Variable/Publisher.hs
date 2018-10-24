module Vest.Bridge.Variable.Publisher
  ( module Vest.Bridge.Variable.Publisher
  ) where

import qualified Stream
import Vest.Bridge.Variable.Prelude
import Vest.DistributedLock
import Vest.Prelude
import Vest.Redis

type family VariableNames spec where
  VariableNames () = '[]
  VariableNames (Variable_ _ _ _ (name :: Symbol) _) = '[ name]
  VariableNames (a
                 :<|> b) = VariableNames a :++ VariableNames b

type family NubVariableNames spec where
  NubVariableNames () = '[]
  NubVariableNames (Variable_ _ _ _ (name :: Symbol) _) = '[ name]
  NubVariableNames (a
                    :<|> b) = Nub (NubVariableNames a :++ NubVariableNames b)

type HasUniqueVariableNames spec = VariableNames spec ~ NubVariableNames spec

type family Variables spec where
  Variables () = ()
  Variables (Variable_ _ _ _ _ a) = Stream a
  Variables (a
             :<|> b) = (Variables a
                        :<|> Variables b)
  -- Would be better to declare like this, but type-level lists cannot be used as argument types:
  -- Variables '[] = '[]
  -- Variables (Variable_ _ _ _ _ a ': variables) = ('[ Streamly.Serial a] :++ Variables variables)
  -- Watch here: https://www.reddit.com/r/haskell/comments/9n0qan/typefamily_monoid/.

class (HasNamespace t) =>
      Publisher t spec
  where
  publish :: t -> Proxy spec -> Variables spec -> IO ()

instance HasNamespace t => Publisher t () where
  publish _ _ _ = return ()

instance ( HasUniqueVariableNames (a
                                   :<|> b)
         , Publisher t a
         , Publisher t b
         ) =>
         Publisher t (a
                      :<|> b) where
  publish t _ (aVariables :<|> bVariables) = do
    publish t (Proxy :: Proxy a) aVariables
    publish t (Proxy :: Proxy b) bVariables

instance ( HasNamespace t
         , HasVariableTransport transport t
         , HasRedisConnection t
         , Serializable fmt a
         , KnownSymbol name
         ) =>
         Publisher t (Variable_ fmt t transport name a) where
  publish t _ stream =
    void . async $ do
      let variableName =
            serialize' @'Pretty $ namespaced @t (Proxy :: Proxy name)
          lockId = retag $ variableName <> "/publisher"
      with @DistributedLock (defaultDistributedLock (redisConnection t) lockId) .
        const $ do
        send <- publishRaw (variableTransport @transport t) variableName
        Stream.mapM_ (send . serialize' @fmt) stream
