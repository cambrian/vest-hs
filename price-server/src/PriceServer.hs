module PriceServer
  ( Config
  , T
  , make
  , priceTezosContract
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
  { latestTezosPrice :: TVar.TVar Money.ExchangeRate "XTZ" "USD"
  }

make :: Config -> IO T
make Config {} = do
  let dummyTezosPrice = Streamly.fromList [1]
  latestTezosPrice <- makeStreamVar' dummyTezosPrice 1
  return T {latestTezosPrice}
-- tezosContractPrice ::
--      Money.ExchangeRate "XTZ" "USD"
--   -> Money.Discrete "XTZ" "mutez"
--   -> DiffTime
--   -> Money.Discrete "USD" "cent"
-- tezosContractPrice xtzUsd size duration =
--   let mutez = toRational $ Money.denseFromDiscrete size
--       seconds = diffTimeToMicros duration / 1000000
--    in 5
-- priceTezosContract ::
--      T
--   -> Money.Discrete "XTZ" "mutez"
--   -> DiffTime
--   -> IO (Money.Discrete "USD" "cent")
-- priceTezosContract T {latestTezosPrice} size duration = do
--   let mutez = toRational $ Money.denseFromDiscrete size
--   tezosPrice <- TVar.readTVarIO latestTezosPrice
--   let price = mutez * tezosPrice
--   return $ fst $ Money.discreteFromDense Money.Ceiling (Money.dense' price)
