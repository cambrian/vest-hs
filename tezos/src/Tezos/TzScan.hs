module Tezos.TzScan
  ( module Tezos.TzScan
  ) where

import Data.Aeson
import Data.Aeson.Encoding.Internal
import qualified Data.ByteString.Lazy as Lazy
import Vest
import Vest.Http

type V1 = "v1"

type V2 = "v2"

type Direct result = Get '[ JSON] result

-- We decode uncertain types into Values (TzScan uses strings/ints for big/small numbers).
fromValueUnsafe :: (Read a) => Value -> IO a
fromValueUnsafe value = do
  let valueText =
        decodeUtf8 . Lazy.toStrict . encodingToLazyByteString . toEncoding $
        value
  case read @Text valueText of
    Just valueRaw -> readUnsafe valueRaw
    Nothing -> readUnsafe valueText

data AddressTz = AddressTz
  { tz :: Text
  } deriving (Show, Generic, FromJSON)

data RewardsSplit = RewardsSplit
  { delegate_staking_balance :: Value
  , delegators_nb :: Int
  , delegators_balance :: [(AddressTz, Value)]
  , blocks_rewards :: Value
  , endorsements_rewards :: Value
  , fees :: Value
  , future_blocks_rewards :: Value
  , future_endorsements_rewards :: Value
  , gain_from_denounciation :: Value
  , lost_deposit_from_denounciation :: Value
  , lost_rewards_denounciation :: Value
  , lost_fees_denounciation :: Value
  } deriving (Show, Generic, FromJSON)

type GetRewardsSplit
   = V2 :> "rewards_split" :> Capture "delegate" Text :> QueryParam' '[ Required] "cycle" Int :> QueryParam' '[ Required] "p" Int :> Direct RewardsSplit
