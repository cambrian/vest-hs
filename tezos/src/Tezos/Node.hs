module Tezos.Node
  ( module Tezos.Node
  ) where

import Http
import Vest

mainChain :: Text
mainChain = "main"

headBlock :: Text
headBlock = "head"

type Direct result = Get '[ JSON] result

type Streaming result = StreamGet NewlineFraming JSON (ResultStream result)

type WithChain spec = "chains" :> Capture "chain" Text :> spec

type WithChainBlock spec = WithChain ("blocks" :> Capture "block" Text :> spec)

type WithChainBlockContract spec
   = WithChainBlock ("context" :> "contracts" :> Capture "contract" Text :> spec)

type WithChainBlockDelegate spec
   = WithChainBlock ("context" :> "delegates" :> Capture "delegate" Text :> spec)

data NotifyBlock = NotifyBlock
  { hash :: Text
  , level :: Int
  , proto :: Int
  , predecessor :: Text
  , timestamp :: Text
  , validation_pass :: Int
  , operations_hash :: Text
  , fitness :: [Text]
  , context :: Text
  , protocol_data :: Text
  } deriving (Show, Generic, FromJSON)

type MonitorBlocks = "monitor" :> "heads" :> "main" :> Streaming NotifyBlock

data Constants = Constants
  { proof_of_work_nonce_size :: Int
  , nonce_length :: Int
  , max_revelations_per_block :: Int
  , max_operation_data_length :: Int
  , preserved_cycles :: Int
  , blocks_per_cycle :: Int
  , blocks_per_commitment :: Int
  , blocks_per_roll_snapshot :: Int
  , blocks_per_voting_period :: Int
  , time_between_blocks :: [Text]
  , endorsers_per_block :: Int
  , hard_gas_limit_per_operation :: Text
  , hard_gas_limit_per_block :: Text
  , proof_of_work_threshold :: Text
  , tokens_per_roll :: Text
  , michelson_maximum_type_size :: Int
  , seed_nonce_revelation_tip :: Text
  , origination_burn :: Text
  , block_security_deposit :: Text
  , endorsement_security_deposit :: Text
  , block_reward :: Text
  , endorsement_reward :: Text
  , cost_per_byte :: Text
  , hard_storage_limit_per_operation :: Text
  } deriving (Show, Generic, FromJSON)

type GetConstants
   = WithChainBlock ("context" :> "constants" :> Direct Constants)

type ListContracts = WithChainBlock ("context" :> "contracts" :> Direct [Text])

type GetContractManager = WithChainBlockContract ("manager" :> Direct Text)

type GetContractBalance = WithChainBlockContract ("balance" :> Direct Text)

data FrozenBalance = FrozenBalance
  { cycle :: Int
  , deposit :: Text
  , fees :: Text
  , rewards :: Text
  }

type ListFrozenBalanceCycles
   = WithChainBlockDelegate ("frozen_balance_by_cycle" :> Direct [FrozenBalance])

type ListDelegatedContracts
   = WithChainBlockDelegate ("delegated_contracts" :> Direct [Text])
