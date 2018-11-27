module Vest.Prelude
  ( module Vest.Prelude
  , module Reexports
  ) where

import Vest.Prelude.Core as Reexports
import Vest.Prelude.Counter as Reexports
import Vest.Prelude.Crypto as Reexports
import Vest.Prelude.Loadable as Reexports
import Vest.Prelude.Log as Reexports
import Vest.Prelude.Money as Reexports
import Vest.Prelude.Namespace as Reexports
import Vest.Prelude.Resource as Reexports
import Vest.Prelude.Serialize as Reexports
import Vest.Prelude.Stream as Reexports
import Vest.Prelude.Time as Reexports
import Vest.Prelude.TypeLevel as Reexports
import Vest.Prelude.UUID as Reexports

-- TODO: Find a better home for me?
gapFilledStream ::
     Indexable a
  => (IndexOf a -> IO a)
  -> IndexOf a
  -> Stream QueueBuffer a
  -> IO (Stream QueueBuffer a)
gapFilledStream materializer startIndex stream = do
  (writer, masterStream) <- newStream
  let f a idx =
        case compare idx (index a) of
          LT -> do
            a' <- materializer idx
            void $ writeStream writer a'
            f a $ succ idx
          EQ -> do
            void $ writeStream writer a
            return $ succ $ index a
          GT -> return idx
  void . async $ foldMStream f startIndex stream >> closeStream writer
  return masterStream
