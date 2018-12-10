-- Class for resources that can be loaded from configs.
--
-- To define loadable data or resources
--
-- data A
--
-- instance Loadable A where
--   configName = [relfile|a.yaml|]
--
-- To use:
--
-- withLoadable configDirs $ \(a :<|> b) -> do
--   ...
--
-- For our loadable A, this will look for "a.yaml" within configDirs.
--
-- When dependent typing comes out, make configFile a type
module Vest.Prelude.Loadable
  ( SimpleLoadable(..)
  , SplitLoadable(..)
  , Loadable(load)
  , withLoadable
  , Specific(..)
  ) where

import Vest.Prelude.Core
import Vest.Prelude.Namespace
import Vest.Prelude.Resource
import Vest.Prelude.TypeLevel

data MissingFileException = MissingFileException
  { file :: Path Rel File
  , searchedPaths :: [Path Abs Dir]
  } deriving (Eq, Show, Exception)

class FromJSON a =>
      SimpleLoadable a
  where
  file :: Path Rel File
  -- ^ TODO: it would be nice to have a type-level file, so long as it still can be checked at compile
  -- time.
  configPathUnsafe :: [Path Abs Dir] -> IO (Path Abs File)
  configPathUnsafe searchPaths =
    findFile searchPaths (file @a) >>=
    fromJustUnsafe (MissingFileException (file @a) searchPaths)

class (Loadable (LoadableParts a)) =>
      SplitLoadable a
  where
  type LoadableParts a
  composeLoadableParts :: LoadableParts a -> a

data LoadableType
  = Simple_
  | Split_

data LoadableItem (t :: LoadableType) a

type family LoadableResult spec where
  LoadableResult (LoadableItem _ a) = a
  LoadableResult (a
                  :<|> b) = (LoadableResult a
                             :<|> LoadableResult b)

-- The Loadable and Loadable_ classes are a hack to enable multiple default implementations.
-- Unfortunately it doesn't seem to work. Waiting for GHC 8.6 to change this.
class Loadable a where
  load :: [Path Abs Dir] -> IO a
  default load :: forall t. Loadable_ t a =>
                              [Path Abs Dir] -> IO a
  load = load_ @t

instance (Loadable a, Loadable b) =>
         Loadable (a
                   :<|> b) where
  load paths = do
    aThread <- asyncDetached $ load @a paths
    bThread <- asyncDetached $ load @b paths
    (a, b) <- waitBoth aThread bThread
    return $ a :<|> b

class Loadable_ (t :: LoadableType) a where
  load_ :: [Path Abs Dir] -> IO a

instance SimpleLoadable a => Loadable_ 'Simple_ a where
  load_ paths = configPathUnsafe @a paths >>= readYamlFile

instance SplitLoadable a => Loadable_ 'Split_ a where
  load_ paths = composeLoadableParts <$> load @(LoadableParts a) paths

class LoadableResource a where
  withLoadable :: [Path Abs Dir] -> (a -> IO b) -> IO b

-- TODO: also add instance for pure Loadable
instance (Resource a, Loadable (ResourceConfig a)) => LoadableResource a where
  withLoadable paths = bracket (load paths >>= makeLogged @a) (cleanupLogged @a)

-- | If you have a Loadable T, you also have a Loadable (Specific svc T)
-- where @load will look for servicedir/config-file.yaml instead of just config-file.yaml
-- TODO: update to support split loading
newtype Specific s a = Specific
  { base :: a
  } deriving (Generic) deriving newtype (FromJSON)

instance (HasNamespace s, Resource a) => Resource (Specific s a) where
  type ResourceConfig (Specific s a) = ResourceConfig a
  makeLogged cfg = Specific <$> makeLogged cfg
  cleanupLogged = cleanupLogged . base
  resourceName = namespace @s <> "-" <> resourceName @a

instance (HasNamespace s, SimpleLoadable a) =>
         SimpleLoadable (Specific s a) where
  file = namespaceDir @s </> file @a
