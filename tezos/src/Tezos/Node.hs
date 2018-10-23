module Tezos.Node
  ( module Tezos.Node
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
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

data DelegationOp = DelegationOp
  { kind :: Text
  , source :: Text
  , fee :: Text
  , counter :: Text
  , gas_limit :: Text
  , storage_limit :: Text
  , delegate :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data OriginationOp = OriginationOp
  { kind :: Text
  , source :: Text
  , fee :: Text
  , counter :: Text
  , gas_limit :: Text
  , storage_limit :: Text
  , managerPubkey :: Text
  , balance :: Text
  , spendable :: Maybe Bool
  , delegatable :: Maybe Bool
  , delegate :: Maybe Text
  , script :: Maybe Value
  } deriving (Show, Generic, FromJSON)

data TransactionOp = TransactionOp
  { kind :: Text
  , source :: Text
  , fee :: Text
  , counter :: Text
  , gas_limit :: Text
  , storage_limit :: Text
  , amount :: Text
  , destination :: Text
  , parameters :: Maybe Value
  } deriving (Show, Generic, FromJSON)

data UndefinedOp = UndefinedOp
  { kind :: Text
  } deriving (Show, Generic, FromJSON)

data Operation
  = Delegation DelegationOp
  | Origination OriginationOp
  | Transaction TransactionOp
  | Endorsement UndefinedOp
  | Reveal UndefinedOp
  | Ballot UndefinedOp
  | Proposals UndefinedOp
  | ActivateAccount UndefinedOp
  | DoubleBakingEvidence UndefinedOp
  | DoubleEndorsementEvidence UndefinedOp
  deriving (Show, Generic)

-- This manual dispatch is absolute shit but (after much effort and a question on GitHub) remains
-- the best way to actually parse an Operation.
instance FromJSON Operation where
  parseJSON operation =
    case operation of
      Object object ->
        case HashMap.lookup "kind" object of
          Just (String "delegation") -> Delegation <$> parseJSON operation
          Just (String "origination") -> Origination <$> parseJSON operation
          Just (String "transaction") -> Transaction <$> parseJSON operation
          Just (String "endorsement") -> Endorsement <$> parseJSON operation
          Just (String "reveal") -> Reveal <$> parseJSON operation
          Just (String "ballot") -> Ballot <$> parseJSON operation
          Just (String "proposals") -> Proposals <$> parseJSON operation
          Just (String "activate_account") ->
            ActivateAccount <$> parseJSON operation
          Just (String "double_baking_evidence") ->
            DoubleBakingEvidence <$> parseJSON operation
          Just (String "double_endorsement_evidence") ->
            DoubleEndorsementEvidence <$> parseJSON operation
          _ -> fail "unexpected operation kind"
      _ -> fail "unexpected JSON shape"

data OperationGroup = OperationGroup
  { protocol :: Text
  , chain_id :: Text
  , hash :: Text
  , branch :: Text
  , contents :: [Operation]
  , signature :: Text
  } deriving (Show, Generic, FromJSON)

type GetBlockOperations
   = WithChainBlock ("operations" :> Direct [[OperationGroup]])

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

data Delegate = Delegate
  { balance :: Text
  , frozen_balance :: Text
  , frozen_balance_by_cycle :: [FrozenBalance]
  , staking_balance :: Text
  , delegated_contracts :: [Text]
  , delegated_balance :: Text
  , deactivated :: Bool
  , grace_period :: Int
  } deriving (Show, Generic, FromJSON)

type GetDelegate = WithChainBlockDelegate (Direct Delegate)
