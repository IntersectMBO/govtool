{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module VVA.API.Types where

import Control.Lens ((?~), (.~))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Aeson.KeyMap as Aeson (toList)
import Data.Aeson.TH (deriveJSON)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.OpenApi hiding (Info)
import Data.Text hiding (map)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Servant.API (FromHttpApiData, parseUrlPiece, parseQueryParam)
import Text.Read (readMaybe)
import VVA.API.Utils
import VVA.Config
import GHC.Exts (toList)
import qualified Data.Cache as Cache
import qualified VVA.Proposal as Proposal
import Data.Hashable (Hashable)
import Data.Has (Has, getter, modifier)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.Char (isHexDigit)
import VVA.Types (AppError(ValidationError))
import Data.Proxy (Proxy(Proxy))
import Control.Exception (throw)
import Data.Swagger.Internal (SwaggerType(SwaggerString))
import Control.Monad (guard)

newtype HexText = HexText { unHexText :: Text }
  deriving newtype (Show, Eq)

instance FromJSON HexText where
  parseJSON (Aeson.String t) = do
    if (Text.length t `mod` 2 == 1 || Text.any (not . isHexDigit) t)
      then mzero
      else pure $ HexText t

instance ToJSON HexText where
  toJSON (HexText t) = Aeson.String t

-- To use it in routes, we need to be able to parse it from Text:
instance FromHttpApiData HexText where
  parseUrlPiece txt
    | Text.all isHexDigit txt && (Text.length txt `mod` 2 == 0) = Right (HexText txt)
    | otherwise            = Left "Not a valid hex value"


instance ToParamSchema HexText where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & format ?~ "hex"

instance ToSchema HexText where
  declareNamedSchema _ = do
    textSchema <- declareNamedSchema (Proxy :: Proxy Text)
    return $ textSchema
      & name ?~ "HexText"
      & schema . type_ ?~ OpenApiString
      & schema . format ?~ "hex"
      & schema . example ?~ toJSON (HexText "a1b2c3")

data GovActionId = GovActionId
  { govActionIdTxHash :: HexText
  , govActionIdIndex :: Integer
  }
  deriving (Eq)

instance Show GovActionId where
  show GovActionId {..} = Text.unpack (unHexText govActionIdTxHash) <> "#" <> show govActionIdIndex

instance FromJSON GovActionId where
  parseJSON (Aeson.String t) =
    case Text.splitOn "#" t of
      [hash, rest] -> do
        index <- case readMaybe $ Text.unpack rest of
          Just x -> pure x
          _      -> fail (Text.unpack rest <> " is not a number")
        hexHash <- parseJSON $ Aeson.String hash
        pure $ GovActionId hexHash (read $ Text.unpack index)
      _ -> mzero

instance ToJSON GovActionId where
  toJSON g = Aeson.String (Text.pack $ show g)

-- To use it in routes, we need to be able to parse it from Text:
instance FromHttpApiData GovActionId where
  parseUrlPiece t = case Text.splitOn "#" t of
      [hash, rest] -> do
        index <- case readMaybe $ Text.unpack rest of
          Just x -> pure x
          _      -> Left (Text.tail rest <> " is not a number")
        hexHash <- parseUrlPiece hash
        Right $ GovActionId hexHash index
      _ -> Left "Not a valid hash#index value"

exampleGovActionId :: Text
exampleGovActionId = "a8097e35ef32de0b244f372db242a6139524d62e4bae989e8291e9483532a553#0"

instance ToParamSchema GovActionId where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & format ?~ "hash#index"
    & example ?~ Aeson.String exampleGovActionId

instance ToSchema GovActionId where
  declareNamedSchema _ = do
    textSchema <- declareNamedSchema (Proxy :: Proxy Text)
    return $ textSchema
      & name ?~ "GovActionId"
      & schema . type_ ?~ OpenApiString
      & schema . format ?~ "hash#index"
      & schema . example ?~ Aeson.String exampleGovActionId


data GovernanceActionType
  = ParameterChange
  | HardForkInitiation
  | TreasuryWithdrawals
  | NoConfidence
  | NewCommittee
  | NewConstitution
  | InfoAction
  deriving (Eq, Show, Read, Enum, Bounded, Generic)

instance FromJSON GovernanceActionType where
  parseJSON (Aeson.String governanceActionType) = pure $ fromMaybe InfoAction $ readMaybe (Text.unpack governanceActionType)
  parseJSON _ = fail ""

instance ToJSON GovernanceActionType where
  toJSON x = Aeson.String $ Text.pack $ show x

instance ToSchema GovernanceActionType where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <- genericDeclareNamedSchema (fromAesonOptions defaultOptions) proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Governance Action Type"
          & example ?~ toJSON InfoAction

instance FromHttpApiData GovernanceActionType where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x -> Right x
    Nothing -> Left ("incorrect governance action type: " <> t)

instance ToParamSchema GovernanceActionType where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [GovernanceActionType])

data GovernanceActionSortMode
  = SoonestToExpire
  | NewestCreated
  | MostYesVotes
  deriving (Eq, Show, Read, Enum, Bounded, Generic)

instance FromJSON GovernanceActionSortMode where
  parseJSON (Aeson.String governanceActionSortMode) = pure $ fromJust $ readMaybe (Text.unpack governanceActionSortMode)
  parseJSON _ = fail ""

instance ToJSON GovernanceActionSortMode where
  toJSON x = Aeson.String $ Text.pack $ show x

instance ToSchema GovernanceActionSortMode where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <- genericDeclareNamedSchema (fromAesonOptions defaultOptions) proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Governance Action Sort Mode"
          & example ?~ toJSON NewestCreated

instance FromHttpApiData GovernanceActionSortMode where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x -> Right x
    Nothing -> Left ("incorrect governance action sort mode: " <> t)

instance ToParamSchema GovernanceActionSortMode where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [GovernanceActionSortMode])


newtype GovernanceActionDetails = GovernanceActionDetails { getValue :: Value }
  deriving newtype (Show)

instance FromJSON GovernanceActionDetails where
  parseJSON v@(Aeson.Object o) = do
    let kvpList = map snd $ Aeson.toList o
    forM_ kvpList $ \case
      (Aeson.Object _) -> fail "GovernanceActionDetails cannot have nested objects"
      (Aeson.Array a) -> forM_ (toList a) $ \case
        (Aeson.Object _) -> fail "GovernanceActionDetails cannot have nested objects"
        (Aeson.Array _) ->  fail "GovernanceActionDetails cannot have nested arrays"
        _ -> pure ()
      _ -> pure ()
    return $ GovernanceActionDetails v
  parseJSON _ = fail "GovernanceActionDetails has to be an object"

instance ToJSON GovernanceActionDetails where
  toJSON (GovernanceActionDetails g) = g

instance ToSchema GovernanceActionDetails where
    declareNamedSchema _ = pure $ NamedSchema (Just "GovernanceActionDetails") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A simple JSON value, with object type values, and no nested arrays"
        & example
          ?~ toJSON
                ("{\"some_key\": \"some value\", \"some_key2\": [1,2,3]}" :: Text)

data ProposalResponse = ProposalResponse
  { proposalResponseId :: Text,
    proposalResponseTxHash :: HexText,
    proposalResponseIndex :: Integer,
    proposalResponseType :: GovernanceActionType,
    proposalResponseDetails :: GovernanceActionDetails,
    proposalResponseExpiryDate :: Maybe UTCTime,
    proposalResponseCreatedDate :: UTCTime,
    proposalResponseUrl :: Text,
    proposalResponseMetadataHash :: HexText,
    proposalResponseYesVotes :: Integer,
    proposalResponseNoVotes :: Integer,
    proposalResponseAbstainVotes :: Integer
  }
  deriving (Generic, Show)

deriveJSON (jsonOptions "proposalResponse") ''ProposalResponse

exampleProposalResponse :: Text
exampleProposalResponse = "{ \"id\": \"proposalId123\","
                  <> "\"txHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
                  <> "\"index\": 0,"
                  <> "\"type\": \"InfoAction\","
                  <> "\"details\": \"some details\","
                  <> "\"expiryDate\": \"1970-01-01T00:00:00Z\","
                  <> "\"createdDate\": \"1970-01-01T00:00:00Z\","
                  <> "\"url\": \"https://proposal.metadata.xyz\","
                  <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
                  <> "\"yesVotes\": 0,"
                  <> "\"noVotes\": 0,"
                  <> "\"abstainVotes\": 0}"

instance ToSchema ProposalResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "proposalResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Proposal Response"
          & example
            ?~ toJSON exampleProposalResponse

exampleListProposalsResponse :: Text
exampleListProposalsResponse =
   "{ \"page\": 0,"
  <> "\"pageSize\": 1,"
  <> "\"total\": 1000,"
  <> "\"elements\": ["
  <> exampleProposalResponse <> "]}"

data ListProposalsResponse = ListProposalsResponse
  { listProposalsResponsePage :: Integer
  , listProposalsResponsePageSize :: Integer
  , listProposalsResponseTotal :: Integer
  , listProposalsResponseElements :: [ProposalResponse]
  } deriving (Generic, Show)

deriveJSON (jsonOptions "listProposalsResponse") ''ListProposalsResponse

instance ToSchema ListProposalsResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "listProposalsResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "ListProposalsResponse"
          & example
            ?~ toJSON exampleListProposalsResponse

data VoteParams = VoteParams
  { voteParamsProposalId :: Text,
    voteParamsDrepId :: HexText,
    voteParamsVote :: Text,
    voteParamsUrl :: Maybe Text,
    voteParamsMetadataHash :: Maybe HexText
  }
  deriving (Generic, Show)

deriveJSON (jsonOptions "voteParams") ''VoteParams

exampleVoteParams :: Text
exampleVoteParams =
   "{ \"proposalId\": \"proposalId123\","
  <> "\"drepId\": \"b4e4184bfedf920fec53cdc327de4da661ae427784c0ccca9e3c2f50\","
  <> "\"vote\": \"yes\","
  <> "\"url\": \"https://vote.metadata.xyz\","
  <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\" }"

instance ToSchema VoteParams where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "voteParams"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Vote"
          & example
            ?~ toJSON exampleVoteParams

data VoteResponse = VoteResponse
  { voteResponseVote :: VoteParams,
    voteResponseProposal :: ProposalResponse
  }
  deriving (Generic, Show)

deriveJSON (jsonOptions "voteResponse") ''VoteResponse

exampleVoteResponse :: Text
exampleVoteResponse =
    "{\"vote\": " <> exampleVoteParams <> ","
  <> "\"proposal\": " <> exampleProposalResponse <> "}"

instance ToSchema VoteResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "voteResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Vote Response"
          & example
            ?~ toJSON exampleVoteResponse

data DRepInfoResponse = DRepInfoResponse
  { dRepInfoResponseIsRegistered :: Bool
  , dRepInfoResponseWasRegistered :: Bool
  , dRepInfoResponseDeposit :: Maybe Integer
  } deriving (Generic, Show)

deriveJSON (jsonOptions "dRepInfoResponse") ''DRepInfoResponse

exampleDRepInfoResponse :: Text
exampleDRepInfoResponse =
    "{\"isRegistered\": false,"
  <> "\"wasRegistered\": true,"
  <> "\"deposit\": 2000000}"

instance ToSchema DRepInfoResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
      ( fromAesonOptions $ jsonOptions "dRepInfoResponse" )
      proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "DRep Info Response"
          & example
            ?~ toJSON exampleDRepInfoResponse


data GetProposalResponse = GetProposalResponse
  { getProposalResponseVote :: Maybe VoteParams,
    getProposalResponseProposal :: ProposalResponse
  }
  deriving (Generic, Show)

exampleGetProposalResponse :: Text
exampleGetProposalResponse =
    "{\"vote\": " <> exampleVoteParams <> ","
  <> "\"proposal\": " <> exampleProposalResponse <> "}"


deriveJSON (jsonOptions "getProposalResponse") ''GetProposalResponse

instance ToSchema GetProposalResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "getProposalResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "GetProposal Response"
          & example
            ?~ toJSON exampleGetProposalResponse


newtype GetCurrentEpochParamsResponse = GetCurrentEpochParamsResponse { getCurrentEpochParamsResponse :: Maybe Value }
  deriving newtype (Show)

instance FromJSON GetCurrentEpochParamsResponse where
  parseJSON = pure . GetCurrentEpochParamsResponse . Just

instance ToJSON GetCurrentEpochParamsResponse where
  toJSON (GetCurrentEpochParamsResponse Nothing) = Null
  toJSON (GetCurrentEpochParamsResponse (Just params)) = toJSON params

exampleGetCurrentEpochParamsResponse :: Text
exampleGetCurrentEpochParamsResponse =
  "{ \"id\":90,\"epoch_no\":90,\"min_fee_a\":44,\"min_fee_b\":155381,\"max_block_size\":90112,\"max_tx_size\":16384,\"max_bh_size\":1100,\"key_deposit\":2000000,\"pool_deposit\":500000000,\"max_epoch\":18,\"optimal_pool_count\":5\r\n00,\"influence\":0.3,\"monetary_expand_rate\":0.003,\"treasury_growth_rate\":0.2,\"decentralisation\":0,\"protocol_major\":8,\"protocol_minor\":0,\"min_utxo_value\":0,\"min_pool_cost\":340000000,\"nonce\":\"\\\\x664c2d0eedc1c\r\n9ee7fc8b4f242c8d13ba17fd31454c84357fa8f1ac62f682cf9\",\"cost_model_id\":2,\"price_mem\":0.0577,\"price_step\":7.21e-05,\"max_tx_ex_mem\":14000000,\"max_tx_ex_steps\":10000000000,\"max_block_ex_mem\":62000000,\"max_bloc\r\nk_ex_steps\":20000000000,\"max_val_size\":5000,\"collateral_percent\":150,\"max_collateral_inputs\":3,\"block_id\":387943,\"extra_entropy\":null,\"coins_per_utxo_size\":4310}"

instance ToSchema GetCurrentEpochParamsResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetCurrentEpochParamsResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Protocol parameters encoded as JSON"
        & example
          ?~ toJSON exampleGetCurrentEpochParamsResponse

newtype GetTransactionStatusResponse
  = GetTransactionStatusResponse
  { getTransactionstatusResponseTransactionConfirmed :: Bool
  } deriving (Generic, Show)


deriveJSON (jsonOptions "getTransactionstatusResponse") ''GetTransactionStatusResponse

exampleGetTransactionStatusResponse :: Text
exampleGetTransactionStatusResponse = "{ \"transactionConfirmed\": True }"

instance ToSchema GetTransactionStatusResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "getTransactionstatusResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "GetTransactionStatus Response"
          & example
            ?~ toJSON exampleGetTransactionStatusResponse

newtype DRepHash = DRepHash Text
  deriving (Generic, Show)

instance FromJSON DRepHash where
  parseJSON (Aeson.String s) = pure $ DRepHash s
  parseJSON x = fail ("expected DRepHash to be a string but got: " <> Char8.unpack (encode x))

instance ToJSON DRepHash where
  toJSON (DRepHash raw) = toJSON raw


exampleDrepHash :: Text
exampleDrepHash = "b4e4184bfedf920fec53cdc327de4da661ae427784c0ccca9e3c2f50"

instance ToSchema DRepHash where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepHash") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Hash of a DRep"
        & example
          ?~ toJSON exampleDrepHash




data DRep = DRep
  { dRepDrepId :: DRepHash
  , dRepUrl :: Maybe Text
  , dRepMetadataHash :: Maybe Text
  , dRepDeposit :: Integer
  } deriving (Generic, Show)


deriveJSON (jsonOptions "dRep") ''DRep

exampleDrep :: Text
exampleDrep =
   "{\"drepId\": \"d3a62ffe9c214e1a6a9809f7ab2a104c117f85e1f171f8f839d94be5\","
 <> "\"url\": \"https://proposal.metadata.xyz\","
 <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
 <> "\"deposit\": 0}"

instance ToSchema DRep where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRep") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "DRep"
        & example
          ?~ toJSON exampleDrep

data GetNetworkMetricsResponse = GetNetworkMetricsResponse
  { getNetworkMetricsResponseCurrentTime :: UTCTime
  , getNetworkMetricsResponseCurrentEpoch :: Integer
  , getNetworkMetricsResponseCurrentBlock :: Integer
  , getNetworkMetricsResponseUniqueDelegators :: Integer
  , getNetworkMetricsResponseTotalDelegations :: Integer
  , getNetworkMetricsResponseTotalGovernanceActions :: Integer
  , getNetworkMetricsResponseTotalDRepVotes :: Integer
  , getNetworkMetricsResponseTotalRegisteredDReps :: Integer
  , getNetworkMetricsResponseAlwaysAbstainVotingPower :: Integer
  , getNetworkMetricsResponseAlwaysNoConfidenceVotingPower :: Integer
  }

deriveJSON (jsonOptions "getNetworkMetricsResponse") ''GetNetworkMetricsResponse

exampleGetNetworkMetricsResponse :: Text
exampleGetNetworkMetricsResponse =
   "{\"currentTime\": \"1970-01-01T00:00:00Z\","
 <> "\"currentEpoch\": 0,"
 <> "\"currentBlock\": 0,"
 <> "\"uniqueDelegators\": 0,"
 <> "\"totalDelegations\": 0,"
 <> "\"totalGovernanceActions\": 0,"
 <> "\"totalDRepVotes\": 0,"
 <> "\"totalRegisteredDReps\": 0,"
 <> "\"alwaysAbstainVotingPower\": 0,"
 <> "\"alwaysNoConfidenceVotingPower\": 0}"

instance ToSchema GetNetworkMetricsResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetNetworkMetricsResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetNetworkMetricsResponse"
        & example
          ?~ toJSON exampleGetNetworkMetricsResponse