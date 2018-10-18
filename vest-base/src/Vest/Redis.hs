module Vest.Redis
  ( module Vest.Redis
  ) where

import Database.Redis
import Vest.Prelude.Core
import Vest.Prelude.Resource

data T = T
  { conn :: Connection
  }

localConfig :: ConnectInfo
localConfig = defaultConnectInfo -- Uses localhost:6379.

data TransactionException =
  TransactionException
  deriving (Eq, Ord, Show, Read, Generic, Exception, Hashable, FromJSON, ToJSON)

instance Resource T where
  type ResourceConfig T = ConnectInfo
  make :: ConnectInfo -> IO T
  make config = do
    conn <- checkedConnect config
    return T {conn}
  cleanup :: T -> IO ()
  cleanup T {conn} = disconnect conn
