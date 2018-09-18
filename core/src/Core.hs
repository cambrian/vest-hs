module Core
  (
  ) where

import qualified Bridge
import qualified Core.Db as Db
import qualified Generics.SOP as SOP
import qualified PriceServer
import VestPrelude
import qualified VestPrelude.Money as Money

-- TODO: could be better to use Money.Discrete, but making it play nice with [Eq, Read, Show]
-- requires an unacceptable quantity of boilerplate.
data User (currency :: Symbol) = User
  { hash :: Hash
  , balance :: Money.Dense currency
  } deriving (Eq, Read, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data StakingContract (currency :: Symbol) = StakingContract
  { id :: Id
  , owner :: Hash
  , size :: Money.Dense currency
  , startTime :: Timestamp
  , duration :: Time Day
  , price :: Money.Discrete "USD" "cent" -- not used for calculations but stored for user reference
  } deriving (Eq, Read, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

-- A collection transaction, made on behalf of the owner to an arbitrary recipient address
data Collection (currency :: Symbol) = Collection
  { txHash :: Hash
  , owner :: Hash
  , recipient :: Hash
  , size :: Money.Dense currency
  , time :: Timestamp
  } deriving (Eq, Read, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

-- A staking reward event, for a specific staking contract
data StakingReward (currency :: Symbol) = StakingReward
  { id :: Id
  , tx_hash :: Hash -- the staking reward transaction that this reward is part of
  , staking_contract :: Id
  , size :: Money.Dense currency
  , time :: Timestamp
  } deriving (Eq, Read, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data Config = Config
  {
  } deriving (Eq, Show, Read, Generic)

data T = T
  {
  }

data SaleRequest (a :: Symbol) = SaleRequest
  { user :: Hash
  , size :: Money.Dense a
  , duration :: Time Day
  , payment :: ()
  } deriving (Eq, Read, Show)

class (PriceServer.Priceable a) =>
      Stakeable a
  where
  saleEndpoint :: Proxy a -> Route
  saleEndpoint = Route . ("sale" <>) . proxyText
  -- storeNewContract ::
  --      (MonadSelda m)
  --   => Proxy a
  --   -> SeldaConnection
  --   -> Hash
  --   -> Money.Dense a
  --   -> Timestamp
  --   -> Days
  --   -> Money.Discrete "USD" "cent"
  --   -> m Id
  -- storeNewContract _ db owner size startTime duration price = do
  --   id <- liftIO newUuid
  --   insert_ users (Db.Contract {id, owner, size, startTime})
  --   return id
  -- serve :: Proxy a -> Db.SeldaConnection -> Bridge.T -> T -> IO ()
  -- serve proxy db bridge t = do
  --   Bridge.serveRPC
  --     bridge
  --     (saleEndpoint proxy)
  --     (\SaleRequest {user, size, duration, payment} -> do
  --        price <-
  --          Bridge.callRPC
  --            @(PriceServer.PriceContractRequest a)
  --            @(Money.Dense a)
  --            bridge
  --            (PriceServer.priceContractEndpoint proxy)
  --            (PriceServer.PriceContractRequest {size, duration})
  --        -- do payment
  --        insert_
  --        return ())

make :: Config -> Bridge.T -> IO T
make Config {} bridge = do
  return $ T {}
-- start :: Db.PGConnectInfo -> Bridge.Config -> Config -> IO ()
-- start dbConfig bridgeConfig config =
--   Db.withForever
--     dbConfig
--     (\db -> do
--        Bridge.withForever
--          bridgeConfig
--          (\bridge -> do
--             t <- make config bridge
--             return ()))
