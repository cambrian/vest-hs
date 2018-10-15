module Stream
  ( module Stream
  ) where

import Streamly.Prelude as Stream
-- This module exists so people can do the following:
--
-- import Vest
-- import qualified Stream
-- map :: (a -> b) -> Stream a -> Stream b
-- map = Stream.map
--
-- instead of:
--
-- import qualified Streamly
-- import qualified Streamly.Prelude as Streamly
-- map :: (a -> b) -> Streamly.Serial a -> Streamly.Serial b
-- map = Streamly.map
--
-- For documentation, see Streamly.Prelude on Hackage
