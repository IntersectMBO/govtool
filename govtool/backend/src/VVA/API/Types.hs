{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module VVA.API.Types where

import           Control.Exception          (throw)
import           Control.Lens               ((.~), (?~))
import           Control.Monad              (guard)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.KeyMap          as Aeson (toList)
import           Data.Aeson.TH              (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Cache                 as Cache
import           Data.Char                  (isHexDigit)
import           Data.Function              ((&))
import           Data.Has                   (Has, getter, modifier)
import           Data.Hashable              (Hashable)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.OpenApi               hiding (Info)
import           Data.Pool                  (Pool)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Swagger.Internal      (SwaggerType (SwaggerString))
import           Data.Text                  hiding (map)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.Time

import           Database.PostgreSQL.Simple (Connection)

import           GHC.Exts                   (toList)
import           GHC.Generics

import           Servant.API                (FromHttpApiData, parseQueryParam, parseUrlPiece)

import           Text.Read                  (readMaybe)

import           VVA.API.Utils
import           VVA.Config
import qualified VVA.Proposal               as Proposal
import           VVA.Types                  (AppError (ValidationError))

newtype HexText
  = HexText { unHexText :: Text }
  deriving newtype (Eq, Show)

instance FromJSON HexText where
  parseJSON (Aeson.String t) = do
    if Text.length t `mod` 2 == 1 || Text.any (not . isHexDigit) t
      then mzero
      else pure $ HexText t

instance ToJSON HexText where
  toJSON (HexText t) = Aeson.String t

-- To use it in routes, we need to be able to parse it from Text:
instance FromHttpApiData HexText where
  parseUrlPiece txt
    | Text.all isHexDigit txt && even (Text.length txt) = Right (HexText txt)
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

newtype AnyValue
  = AnyValue { unAnyValue :: Maybe Value }
  deriving newtype (Show)

instance FromJSON AnyValue where
  parseJSON = pure . AnyValue . Just

instance ToJSON AnyValue where
  toJSON (AnyValue Nothing)       = Null
  toJSON (AnyValue (Just params)) = toJSON params

exampleAnyValue :: Text
exampleAnyValue =
  "{ \"any\": \"value\"}"

instance ToSchema AnyValue where
    declareNamedSchema _ = pure $ NamedSchema (Just "AnyValue") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Any value"
        & example
          ?~ toJSON exampleAnyValue

data MetadataValidationStatus = IncorrectFormat | IncorrectJSONLD | IncorrectHash | UrlNotFound deriving
    ( Eq
    , Show
    )

instance ToJSON MetadataValidationStatus where
  toJSON IncorrectFormat = "INCORRECT_FORMTAT"
  toJSON IncorrectJSONLD = "INVALID_JSONLD"
  toJSON IncorrectHash   = "INVALID_HASH"
  toJSON UrlNotFound     = "URL_NOT_FOUND"

instance FromJSON MetadataValidationStatus where
  parseJSON (String s) = case s of
    "INCORRECT_FORMTAT" -> pure IncorrectFormat
    "INVALID_JSONLD"    -> pure IncorrectJSONLD
    "INVALID_HASH"      -> pure IncorrectHash
    "URL_NOT_FOUND"     -> pure UrlNotFound
    _                   -> fail "Invalid MetadataValidationStatus"
  parseJSON _ = fail "Invalid MetadataValidationStatus"

instance ToSchema MetadataValidationStatus where
    declareNamedSchema _ = pure $ NamedSchema (Just "MetadataValidationStatus") $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "Metadata Validation Status"
        & enum_ ?~ map toJSON [IncorrectFormat, IncorrectJSONLD, IncorrectHash, UrlNotFound]



data InternalMetadataValidationResponse
  = InternalMetadataValidationResponse
      { internalMetadataValidationResponseStatus :: Maybe MetadataValidationStatus
      , internalMmetadataValidationResponseValid :: Bool
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "internalMetadataValidationResponse") ''InternalMetadataValidationResponse

instance ToSchema InternalMetadataValidationResponse where
    declareNamedSchema _ = do
      NamedSchema name_ schema_ <-
        genericDeclareNamedSchema
        ( fromAesonOptions $ jsonOptions "internalMetadataValidationResponse" )
        (Proxy :: Proxy InternalMetadataValidationResponse)
      return $
        NamedSchema name_ $
          schema_
            & description ?~ "Metadata Validation Response"
            & example
              ?~ toJSON ("{\"status\": \"INCORRECT_FORMTAT\", \"valid\":false, \"raw\":{\"some\":\"key\"}}" :: Text)


data MetadataValidationResponse
  = MetadataValidationResponse
      { metadataValidationResponseStatus :: Maybe Text
      , metadataValidationResponseValid  :: Bool
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "metadataValidationResponse") ''MetadataValidationResponse

instance ToSchema MetadataValidationResponse where
    declareNamedSchema _ = do
      NamedSchema name_ schema_ <-
        genericDeclareNamedSchema
        ( fromAesonOptions $ jsonOptions "metadataValidationResponse" )
        (Proxy :: Proxy MetadataValidationResponse)
      return $
        NamedSchema name_ $
          schema_
            & description ?~ "Metadata Validation Response"
            & example
              ?~ toJSON ("{\"status\": \"INCORRECT_FORMTAT\", \"valid\":false}" :: Text)

data MetadataValidationParams
  = MetadataValidationParams
      { metadataValidationParamsUrl  :: Text
      , metadataValidationParamsHash :: HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "metadataValidationParams") ''MetadataValidationParams

instance ToSchema MetadataValidationParams where
    declareNamedSchema proxy = do
      NamedSchema name_ schema_ <-
        genericDeclareNamedSchema
        ( fromAesonOptions $ jsonOptions "metadataValidationParams" )
        proxy
      return $
        NamedSchema name_ $
          schema_
            & description ?~ "Metadata Validation Params"
            & example
              ?~ toJSON ("{\"url\": \"https://metadata.xyz\", \"hash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\"}" :: Text)



data GovActionId
  = GovActionId
      { govActionIdTxHash :: HexText
      , govActionIdIndex  :: Integer
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


data GovernanceActionType = ParameterChange | HardForkInitiation | TreasuryWithdrawals | NoConfidence | NewCommittee | NewConstitution | InfoAction deriving
    ( Bounded
    , Enum
    , Eq
    , Generic
    , Read
    , Show
    )

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
    Just x  -> Right x
    Nothing -> Left ("incorrect governance action type: " <> t)

instance ToParamSchema GovernanceActionType where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [GovernanceActionType])


data DRepSortMode = VotingPower | RegistrationDate | Status deriving (Bounded, Enum, Eq, Generic, Read, Show)

instance FromJSON DRepSortMode where
  parseJSON (Aeson.String dRepSortMode) = pure $ fromJust $ readMaybe (Text.unpack dRepSortMode)
  parseJSON _                           = fail ""

instance ToJSON DRepSortMode where
  toJSON x = Aeson.String $ Text.pack $ show x

instance ToSchema DRepSortMode where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <- genericDeclareNamedSchema (fromAesonOptions defaultOptions) proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "DRep Sort Mode"
          & example ?~ toJSON VotingPower

instance FromHttpApiData DRepSortMode where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x  -> Right x
    Nothing -> Left ("incorrect DRep sort mode: " <> t)

instance ToParamSchema DRepSortMode where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [DRepSortMode])


data GovernanceActionSortMode = SoonestToExpire | NewestCreated | MostYesVotes deriving
    ( Bounded
    , Enum
    , Eq
    , Generic
    , Read
    , Show
    )

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
    Just x  -> Right x
    Nothing -> Left ("incorrect governance action sort mode: " <> t)

instance ToParamSchema GovernanceActionSortMode where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [GovernanceActionSortMode])


newtype GovernanceActionDetails
  = GovernanceActionDetails { getValue :: Value }
  deriving newtype (Show)

instance FromJSON GovernanceActionDetails where
  parseJSON v@(Aeson.Object o) = do
    let kvpList = map snd $ Aeson.toList o
    forM_ kvpList $ \case
      (Aeson.Object _) -> fail "GovernanceActionDetails cannot have nested objects"
      (Aeson.Array a) -> forM_ (toList a) $ \case
        (Aeson.Object _) -> fail "GovernanceActionDetails cannot have nested objects"
        (Aeson.Array _)  ->  fail "GovernanceActionDetails cannot have nested arrays"
        _                -> pure ()
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


newtype GovernanceActionMetadata
  = GovernanceActionMetadata Value
  deriving newtype (Show)

instance FromJSON GovernanceActionMetadata where
  parseJSON v@(Aeson.Object o) = pure (GovernanceActionMetadata v)
  parseJSON _                  = fail "GovernanceActionMetadata has to be an object"

instance ToJSON GovernanceActionMetadata where
  toJSON (GovernanceActionMetadata g) = g

instance ToSchema GovernanceActionMetadata where
    declareNamedSchema _ = pure $ NamedSchema (Just "GovernanceActionMetadata") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A Governance Action metadata"
        & example
          ?~ toJSON
                ("{\"some_key\": \"some value\", \"some_key2\": [1,2,3]}" :: Text)



newtype GovernanceActionReferences
  = GovernanceActionReferences Value
  deriving newtype (Show)

instance FromJSON GovernanceActionReferences where
  parseJSON v@(Aeson.Array a) = pure (GovernanceActionReferences v)
  parseJSON _                 = fail "GovernanceActionReferences has to be an array"

instance ToJSON GovernanceActionReferences where
  toJSON (GovernanceActionReferences g) = g

instance ToSchema GovernanceActionReferences where
    declareNamedSchema _ = pure $ NamedSchema (Just "GovernanceActionReferences") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A Governance Action References"
        & example
          ?~ toJSON
                ("[{\"uri\": \"google.com\", \"@type\": \"Other\", \"label\": \"example label\"}]" :: Text)



data ProposalResponse
  = ProposalResponse
      { proposalResponseId             :: Text
      , proposalResponseTxHash         :: HexText
      , proposalResponseIndex          :: Integer
      , proposalResponseType           :: GovernanceActionType
      , proposalResponseDetails        :: Maybe GovernanceActionDetails
      , proposalResponseExpiryDate     :: Maybe UTCTime
      , proposalResponseExpiryEpochNo  :: Maybe Integer
      , proposalResponseCreatedDate    :: UTCTime
      , proposalResponseCreatedEpochNo :: Integer
      , proposalResponseUrl            :: Text
      , proposalResponseMetadataHash   :: HexText
      , proposalResponseTitle          :: Maybe Text
      , proposalResponseAbstract       :: Maybe Text
      , proposalResponseMotivation     :: Maybe Text
      , proposalResponseRationale      :: Maybe Text
      , proposalResponseMetadata       :: Maybe GovernanceActionMetadata
      , proposalResponseReferences     :: [Text]
      , proposalResponseYesVotes       :: Integer
      , proposalResponseNoVotes        :: Integer
      , proposalResponseAbstainVotes   :: Integer
      , proposalResponseMetadataStatus :: Maybe Text
      , proposalResponseMetadataValid  :: Bool
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
                  <> "\"expiryEpochNo\": 0,"
                  <> "\"createdDate\": \"1970-01-01T00:00:00Z\","
                  <> "\"createdEpochNo\": 0,"
                  <> "\"url\": \"https://proposal.metadata.xyz\","
                  <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
                  <> "\"title\": \"Proposal Title\","
                  <> "\"abstract\": \"Proposal About\","
                  <> "\"motivation\": \"Proposal Motivation\","
                  <> "\"rationale\": \"Proposal Rationale\","
                  <> "\"metadata\": {\"key\": \"value\"},"
                  <> "\"references\": [\"google.com\"],"
                  <> "\"yesVotes\": 0,"
                  <> "\"noVotes\": 0,"
                  <> "\"abstainVotes\": 0,"
                  <> "\"metadataStatus\": \"URL_NOT_FOUND\","
                  <> "\"metadataValid\": true}"

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

data ListProposalsResponse
  = ListProposalsResponse
      { listProposalsResponsePage     :: Integer
      , listProposalsResponsePageSize :: Integer
      , listProposalsResponseTotal    :: Integer
      , listProposalsResponseElements :: [ProposalResponse]
      }
  deriving (Generic, Show)

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

data VoteParams
  = VoteParams
      { voteParamsProposalId   :: Text
      , voteParamsDrepId       :: HexText
      , voteParamsVote         :: Text
      , voteParamsUrl          :: Maybe Text
      , voteParamsMetadataHash :: Maybe HexText
      , voteParamsEpochNo      :: Integer
      , voteParamsDate         :: UTCTime
      , voteParamsTxHash       :: HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "voteParams") ''VoteParams

exampleVoteParams :: Text
exampleVoteParams =
   "{ \"proposalId\": \"proposalId123\","
  <> "\"drepId\": \"b4e4184bfedf920fec53cdc327de4da661ae427784c0ccca9e3c2f50\","
  <> "\"vote\": \"yes\","
  <> "\"url\": \"https://vote.metadata.xyz\","
  <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
  <> "\"epochNo\": 0,"
  <> "\"date\": \"1970-01-01T00:00:00Z\","
  <> "\"txHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\"}"

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

data VoteResponse
  = VoteResponse
      { voteResponseVote     :: VoteParams
      , voteResponseProposal :: ProposalResponse
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

data DRepInfoResponse
  = DRepInfoResponse
      { dRepInfoResponseIsRegisteredAsDRep       :: Bool
      , dRepInfoResponseWasRegisteredAsDRep      :: Bool
      , dRepInfoResponseIsRegisteredAsSoleVoter  :: Bool
      , dRepInfoResponseWasRegisteredAsSoleVoter :: Bool
      , dRepInfoResponseDeposit                  :: Maybe Integer
      , dRepInfoResponseUrl                      :: Maybe Text
      , dRepInfoResponseDataHash                 :: Maybe HexText
      , dRepInfoResponseVotingPower              :: Maybe Integer
      , dRepInfoResponseDRepRegisterTxHash       :: Maybe HexText
      , dRepInfoResponseDRepRetireTxHash         :: Maybe HexText
      , dRepInfoResponseSoleVoterRegisterTxHash  :: Maybe HexText
      , dRepInfoResponseSoleVoterRetireTxHash    :: Maybe HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "dRepInfoResponse") ''DRepInfoResponse

exampleDRepInfoResponse :: Text
exampleDRepInfoResponse =
    "{\"isRegisteredAsDRep\": false,"
  <> "\"wasRegisteredAsDRep\": true,"
  <> "\"isRegisteredAsSoleVoter\": true,"
  <> "\"wasRegisteredAsSoleVoter\": true,"
  <> "\"deposit\": 2000000,"
  <> "\"url\": \"https://drep.metadata.xyz\","
  <> "\"dataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
  <> "\"votingPower\": 1000000,"
  <> "\"dRepRegisterTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"dRepRetireTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"soleVoterRegisterTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"soleVoterRetireTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\"}"

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


data GetProposalResponse
  = GetProposalResponse
      { getProposalResponseVote     :: Maybe VoteParams
      , getProposalResponseProposal :: ProposalResponse
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


newtype GetCurrentEpochParamsResponse
  = GetCurrentEpochParamsResponse { getCurrentEpochParamsResponse :: Maybe Value }
  deriving newtype (Show)

instance FromJSON GetCurrentEpochParamsResponse where
  parseJSON = pure . GetCurrentEpochParamsResponse . Just

instance ToJSON GetCurrentEpochParamsResponse where
  toJSON (GetCurrentEpochParamsResponse Nothing)       = Null
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
  = GetTransactionStatusResponse { getTransactionstatusResponseTransactionConfirmed :: Bool }
  deriving (Generic, Show)


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

newtype DRepHash
  = DRepHash Text
  deriving (Generic, Show)

instance FromJSON DRepHash where
  parseJSON (Aeson.String s) = pure $ DRepHash s
  parseJSON x                = fail ("expected DRepHash to be a string but got: " <> Char8.unpack (encode x))

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


data DRepStatus = Active | Inactive | Retired deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- ToJSON instance for DRepStatus
instance ToJSON DRepStatus where
  toJSON Retired  = "Retired"
  toJSON Active   = "Active"
  toJSON Inactive = "Inactive"

-- FromJSON instance for DRepStatus
instance FromJSON DRepStatus where
  parseJSON = withText "DRepStatus" $ \case
    "Retired"  -> pure Retired
    "Active"   -> pure Active
    "Inactive" -> pure Inactive
    _          -> fail "Invalid DRepStatus"

-- ToSchema instance for DRepStatus
instance ToSchema DRepStatus where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepStatus") $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "DRep Status"
        & enum_ ?~ map toJSON [Retired, Active, Inactive]

instance FromHttpApiData DRepStatus where
  parseQueryParam t = case readMaybe $ Text.unpack t of
    Just x  -> Right x
    Nothing -> Left ("incorrect DRep status " <> t)

instance ToParamSchema DRepStatus where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ map toJSON (enumFromTo minBound maxBound :: [DRepStatus])



data DRepType = NormalDRep | SoleVoter

instance Show DRepType where
  show NormalDRep = "DRep"
  show SoleVoter  = "SoleVoter"

-- ToJSON instance for DRepType
instance ToJSON DRepType where
  toJSON NormalDRep = "DRep"
  toJSON SoleVoter  = "SoleVoter"

-- FromJSON instance for DRepType
instance FromJSON DRepType where
  parseJSON = withText "DRepType" $ \case
    "DRep"      -> pure NormalDRep
    "SoleVoter" -> pure SoleVoter
    _           -> fail "Invalid DRepType"

-- ToSchema instance for DRepType
instance ToSchema DRepType where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepType") $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "DRep Type"
        & enum_ ?~ map toJSON [NormalDRep, SoleVoter]

data DRep
  = DRep
      { dRepDrepId                 :: DRepHash
      , dRepView                   :: Text
      , dRepUrl                    :: Maybe Text
      , dRepMetadataHash           :: Maybe Text
      , dRepDeposit                :: Integer
      , dRepVotingPower            :: Maybe Integer
      , dRepStatus                 :: DRepStatus
      , dRepType                   :: DRepType
      , dRepLatestTxHash           :: Maybe HexText
      , dRepLatestRegistrationDate :: UTCTime
      , dRepBio                    :: Maybe Text
      , dRepDRepName               :: Maybe Text
      , dRepEmail                  :: Maybe Text
      , dRepReferences             :: [Text]
      , dRepMetadataStatus         :: Maybe Text
      , dRepMetadataValid          :: Bool
      }
  deriving (Generic, Show)


deriveJSON (jsonOptions "dRep") ''DRep

exampleDrep :: Text
exampleDrep =
     "{\"drepId\": \"d3a62ffe9c214e1a6a9809f7ab2a104c117f85e1f171f8f839d94be5\","
  <> "\"view\": \"drep1l8uyy66sm8u82h82gc8hkcy2xu24dl8ffsh58aa0v7d37yp48u8\","
  <> "\"url\": \"https://proposal.metadata.xyz\","
  <> "\"metadataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
  <> "\"deposit\": 0,"
  <> "\"votingPower\": 0,"
  <> "\"status\": \"Active\","
  <> "\"type\": \"DRep\","
  <> "\"latestTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"latestRegistrationDate\": \"1970-01-01T00:00:00Z\","
  <> "\"bio\": \"DRep Bio\","
  <> "\"dRepName\": \"DRep Name\","
  <> "\"email\": \"google@gmail.com\","
  <> "\"references\": [\"google.com\"],"
  <> "\"metadataStatus\": \"URL_NOT_FOUND\","
  <> "\"metadataValid\": true}"

-- ToSchema instance for DRep
instance ToSchema DRep where
    declareNamedSchema proxy = do
      NamedSchema name_ schema_ <-
        genericDeclareNamedSchema
        ( fromAesonOptions $ jsonOptions "dRep" )
        proxy
      return $
        NamedSchema name_ $
          schema_
            & description ?~ "DRep"
            & example
              ?~ toJSON exampleDrep


exampleListDRepsResponse :: Text
exampleListDRepsResponse =
   "{ \"page\": 0,"
  <> "\"pageSize\": 1,"
  <> "\"total\": 1000,"
  <> "\"elements\": ["
  <> exampleDrep <> "]}"

data ListDRepsResponse
  = ListDRepsResponse
      { listDRepsResponsePage     :: Integer
      , listDRepsResponsePageSize :: Integer
      , listDRepsResponseTotal    :: Integer
      , listDRepsResponseElements :: [DRep]
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "listDRepsResponse") ''ListDRepsResponse

instance ToSchema ListDRepsResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "listDRepsResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "ListProposalsResponse"
          & example
            ?~ toJSON exampleListDRepsResponse


data DelegationResponse
  = DelegationResponse
      { delegationResponseDRepHash :: Maybe HexText
      , delegationResponseDRepView :: Text
      , delegationResponseTxHash   :: HexText
      }
deriveJSON (jsonOptions "delegationResponse") ''DelegationResponse

exampleDelegationResponse :: Text
exampleDelegationResponse = "{\"drepHash\": \"b4e4184bfedf920fec53cdc327de4da661ae427784c0ccca9e3c2f50\","
                          <> "\"drepView\": \"drep1l8uyy66sm8u82h82gc8hkcy2xu24dl8ffsh58aa0v7d37yp48u8\","
                          <> "\"txHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\"}"

instance ToSchema DelegationResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "DelegationResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Delegation Response"
        & example
          ?~ toJSON exampleDelegationResponse

data GetNetworkMetricsResponse
  = GetNetworkMetricsResponse
      { getNetworkMetricsResponseCurrentTime                   :: UTCTime
      , getNetworkMetricsResponseCurrentEpoch                  :: Integer
      , getNetworkMetricsResponseCurrentBlock                  :: Integer
      , getNetworkMetricsResponseUniqueDelegators              :: Integer
      , getNetworkMetricsResponseTotalDelegations              :: Integer
      , getNetworkMetricsResponseTotalGovernanceActions        :: Integer
      , getNetworkMetricsResponseTotalDRepVotes                :: Integer
      , getNetworkMetricsResponseTotalRegisteredDReps          :: Integer
      , getNetworkMetricsResponseAlwaysAbstainVotingPower      :: Integer
      , getNetworkMetricsResponseAlwaysNoConfidenceVotingPower :: Integer
      , getNetworkMetricsResponseNetworkName                   :: Text
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
 <> "\"alwaysNoConfidenceVotingPower\": 0,"
 <> "\"networkName\": \"Mainnet\"}"

instance ToSchema GetNetworkMetricsResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetNetworkMetricsResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetNetworkMetricsResponse"
        & example
          ?~ toJSON exampleGetNetworkMetricsResponse

