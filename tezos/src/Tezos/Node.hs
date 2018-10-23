module Tezos.Node
  ( module Tezos.Node
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Http
import Text.Casing
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

data LevelInfo = LevelInfo
  { level :: Int
  , level_position :: Int
  , cycle :: Int
  , cycle_position :: Int
  , voting_period :: Int
  , voting_period_position :: Int
  , expected_commitment :: Bool
  } deriving (Show, Generic, FromJSON)

type GetBlockLevelInfo
   = WithChainBlock ("helpers" :> "current_level" :> Direct LevelInfo)

-- Items with Aeson types (like Object or Array) are things we don't really care to explicitly
-- specify in Haskell terms (because it's annoying and we don't need them).
data BlockMetadata = BlockMetadata
  { protocol :: Text
  , next_protocol :: Text
  , test_chain_status :: Object
  , max_operations_ttl :: Int
  , max_operation_data_length :: Int
  , max_block_header_length :: Int
  , max_operation_list_length :: Array
  , baker :: Text
  , level :: LevelInfo
  , voting_period_kind :: Text
  , nonce_hash :: Maybe Text
  , consumed_gas :: Text
  , deactivated :: Array
  , balance_updates :: Array
  } deriving (Show, Generic, FromJSON)

type GetBlockMetadata = WithChainBlock ("metadata" :> Direct [BlockMetadata])

-- { "kind": "endorsement",
-- "level": integer ∈ [-2^31-2, 2^31+2] }
-- || { "kind": "seed_nonce_revelation",
--    "level": integer ∈ [-2^31-2, 2^31+2],
--    "nonce": /^[a-zA-Z0-9]+$/ }
-- || { "kind": "double_endorsement_evidence",
--    "op1": $inlined.endorsement,
--    "op2": $inlined.endorsement }
-- || { "kind": "double_baking_evidence",
--    "bh1": $block_header.alpha.full_header,
--    "bh2": $block_header.alpha.full_header }
-- || { "kind": "activate_account",
--    "pkh": $Ed25519.Public_key_hash,
--    "secret": /^[a-zA-Z0-9]+$/ }
-- || { "kind": "proposals",
--    "source": $Signature.Public_key_hash,
--    "period": integer ∈ [-2^31-2, 2^31+2],
--    "proposals": [ $Protocol_hash ... ] }
-- || { "kind": "ballot",
--    "source": $Signature.Public_key_hash,
--    "period": integer ∈ [-2^31-2, 2^31+2],
--    "proposal": $Protocol_hash,
--    "ballot": "nay" | "yay" | "pass" }
-- || { "kind": "reveal",
--    "source": $contract_id,
--    "fee": $mutez,
--    "counter": $positive_bignum,
--    "gas_limit": $positive_bignum,
--    "storage_limit": $positive_bignum,
--    "public_key": $Signature.Public_key }
-- || { "kind": "transaction",
--    "source": $contract_id,
--    "fee": $mutez,
--    "counter": $positive_bignum,
--    "gas_limit": $positive_bignum,
--    "storage_limit": $positive_bignum,
--    "amount": $mutez,
--    "destination": $contract_id,
--    "parameters"?: $micheline.michelson_v1.expression }
-- || { "kind": "origination",
--    "source": $contract_id,
--    "fee": $mutez,
--    "counter": $positive_bignum,
--    "gas_limit": $positive_bignum,
--    "storage_limit": $positive_bignum,
--    "managerPubkey": $Signature.Public_key_hash,
--    "balance": $mutez,
--    "spendable"?: boolean,
--    "delegatable"?: boolean,
--    "delegate"?: $Signature.Public_key_hash,
--    "script"?: $scripted.contracts }
-- || { "kind": "delegation",
--    "source": $contract_id,
--    "fee": $mutez,
--    "counter": $positive_bignum,
--    "gas_limit": $positive_bignum,
--    "storage_limit": $positive_bignum,
--    "delegate"?: $Signature.Public_key_hash }
data Operation
  = Delegation Object
  | Origination Object
  | Transaction Object
  | Endorsement Object
  | Reveal Object
  | Ballot Object
  | Proposals Object
  | ActivateAccount Object
  | DoubleBakingEvidence Object
  | DoubleEndorsementEvidence Object
  deriving (Show, Generic)

$(deriveJSON
    defaultOptions
      { constructorTagModifier = map toLower . toSnake . fromHumps
      , sumEncoding =
          TaggedObject {tagFieldName = "kind", contentsFieldName = "kind"}
      }
    ''Operation)

data OperationGroup = OperationGroup
  { protocol :: Text
  , chain_id :: Text
  , hash :: Text
  , branch :: Text
  , contents :: [Operation]
  , signature :: Text
  } deriving (Show, Generic, FromJSON)

type GetBlockOperations
   = WithChainBlock ("operations" :> Direct [OperationGroup])

data BlockHeader = BlockHeader
  { level :: Int
  , proto :: Int
  , predecessor :: Text
  , timestamp :: Text
  , validation_pass :: Int
  , operations_hash :: Text
  , fitness :: Array
  , context :: Text
  , priority :: Int
  , proof_of_work_nonce :: Text
  , signature :: Text
  } deriving (Show, Generic, FromJSON)

data Block = Block
  { protocol :: Text
  , chain_id :: Text
  , hash :: Text
  , header :: BlockHeader
  , metadata :: BlockMetadata
  , operations :: [[OperationGroup]]
  } deriving (Show, Generic, FromJSON)

type GetBlock = WithChainBlock (Direct Block)

type GetContractManager = WithChainBlockContract ("manager" :> Direct Text)

type GetContractBalance = WithChainBlockContract ("balance" :> Direct Text)

data FrozenBalance = FrozenBalance
  { cycle :: Int
  , deposit :: Text
  , fees :: Text
  , rewards :: Text
  } deriving (Show, Generic, FromJSON)

type ListFrozenBalanceCycles
   = WithChainBlockDelegate ("frozen_balance_by_cycle" :> Direct [FrozenBalance])

type ListDelegatedContracts
   = WithChainBlockDelegate ("delegated_contracts" :> Direct [Text])

type GetStakingBalance
   = WithChainBlockDelegate ("staking_balance" :> Direct Text)
