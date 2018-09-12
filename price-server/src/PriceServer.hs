module PriceServer
  (
  ) where

import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad.STM as STM
import Streamly.Prelude as Streamly
import VestPrelude
import qualified VestPrelude.Money as Money

data Config = Config
  {
  } deriving (Eq, Show, Read)

data T = T
  { latestTezosPrice :: TVar.TVar Double
  }

make :: Config -> IO T
make Config {} = do
  let dummyTezosPrice = Streamly.fromList [1]
  latestTezosPrice <- makeStreamVar' dummyTezosPrice 1
  return T {latestTezosPrice}

priceTezosContract ::
     T
  -> Money.Discrete "XTZ" "mutez"
  -> DiffTime
  -> IO (Money.Discrete "USD" "cent")
priceTezosContract T {latestTezosPrice} size duration = do
  let (Money.Dense "XTZ" mutez) = Money.denseFromDiscrete size
  tezosPrice <- TVar.readTVarIO latestTezosPrice
  return mutez * tezosPrice
