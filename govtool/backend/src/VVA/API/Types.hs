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
        if Text.null rest
          then Left "Missing index in hash#index format"
          else do
            index <- case readMaybe $ Text.unpack rest of
              Just x -> pure x
              _      -> Left (rest <> " is not a number")
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


data DRepSortMode = Random | VotingPower | RegistrationDate | Status deriving (Bounded, Enum, Eq, Generic, Read, Show)

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
  = GovernanceActionDetails { getGovernanceActionValue :: Value }
  deriving newtype (Show)

instance FromJSON GovernanceActionDetails where
  parseJSON v = return $ GovernanceActionDetails v

instance ToJSON GovernanceActionDetails where
  toJSON (GovernanceActionDetails g) = g

instance ToSchema GovernanceActionDetails where
    declareNamedSchema _ = pure $ NamedSchema (Just "GovernanceActionDetails") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A JSON value that can include nested objects and arrays"
        & example ?~ toJSON
            (Aeson.object
              [ "some_key" .= ("some value" :: String)
              , "nested_key" .= Aeson.object ["inner_key" .= (1 :: Int)]
              , "array_key" .= [1, 2, 3 :: Int]
              ])

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


newtype ProtocolParams
  = ProtocolParams { getProtocolParams :: Value }
  deriving newtype (Show)

instance FromJSON ProtocolParams where
  parseJSON = pure . ProtocolParams

instance ToJSON ProtocolParams where
  toJSON (ProtocolParams params) = toJSON params

instance ToSchema ProtocolParams where
    declareNamedSchema _ = pure $ NamedSchema (Just "ProtocolParams") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Protocol parameters encoded as JSON"
        & example
          ?~ toJSON exampleGetCurrentEpochParamsResponse


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
      { proposalResponseId                  :: Text
      , proposalResponseTxHash              :: HexText
      , proposalResponseIndex               :: Integer
      , proposalResponseType                :: GovernanceActionType
      , proposalResponseDetails             :: Maybe GovernanceActionDetails
      , proposalResponseExpiryDate          :: Maybe UTCTime
      , proposalResponseExpiryEpochNo       :: Maybe Integer
      , proposalResponseCreatedDate         :: UTCTime
      , proposalResponseCreatedEpochNo      :: Integer
      , proposalResponseUrl                 :: Text
      , proposalResponseMetadataHash        :: HexText
      , proposalResponseProtocolParams      :: Maybe ProtocolParams
      , proposalResponseTitle               :: Maybe Text
      , proposalResponseAbstract            :: Maybe Text
      , proposalResponseMotivation          :: Maybe Text
      , proposalResponseRationale           :: Maybe Text
      , proposalResponseDRepYesVotes        :: Integer
      , proposalResponseDRepNoVotes         :: Integer
      , proposalResponseDRepAbstainVotes    :: Integer
      , proposalResponsePoolYesVotes        :: Integer
      , proposalResponsePoolNoVotes         :: Integer
      , proposalResponsePoolAbstainVotes    :: Integer
      , proposalResponseCcYesVotes          :: Integer
      , proposalResponseCcNoVotes           :: Integer
      , proposalResponseCcAbstainVotes      :: Integer
      , proposalResponsePrevGovActionIndex  :: Maybe Integer
      , proposalResponsePrevGovActionTxHash :: Maybe HexText
      , proposalResponseJson                :: Maybe Value
      , proposalResponseAuthors             :: Maybe ProposalAuthors
      }
  deriving (Generic, Show)

newtype ProposalAuthors = ProposalAuthors { getProposalAuthors :: Value }
  deriving newtype (Show)

instance FromJSON ProposalAuthors where
  parseJSON v@(Array _) = pure $ ProposalAuthors v
  parseJSON _           = fail "ProposalAuthors must be a JSON array"

instance ToJSON ProposalAuthors where
  toJSON (ProposalAuthors v) = v

instance ToSchema ProposalAuthors where
  declareNamedSchema _ = pure $ NamedSchema (Just "ProposalAuthors") $ mempty
    & type_ ?~ OpenApiArray
    & description ?~ "A JSON array of proposal authors"
    & example ?~ toJSON
        [ object
            [ "name" .= ("Alice" :: Text)
            , "witnessAlgorithm" .= ("algo" :: Text)
            , "publicKey" .= ("key" :: Text)
            , "signature" .= ("sig" :: Text)
            ]
        , object
            [ "name" .= ("Bob" :: Text)
            , "witnessAlgorithm" .= ("algo2" :: Text)
            , "publicKey" .= ("key2" :: Text)
            , "signature" .= ("sig2" :: Text)
            ]
        ]

exampleProposalAuthors :: Text
exampleProposalAuthors =
  "[\
  \ {\"name\": \"Alice\",\
  \  \"witnessAlgorithm\": \"Ed25519\",\
  \  \"publicKey\": \"abcdef123456\",\
  \  \"signature\": \"deadbeef\"},\
  \ {\"name\": \"Bob\",\
  \  \"witnessAlgorithm\": \"Ed25519\",\
  \  \"publicKey\": \"123456abcdef\",\
  \  \"signature\": \"beefdead\"}\
  \]"

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
                  <> "\"protocolParams\": " <> exampleGetCurrentEpochParamsResponse <> ","
                  <> "\"title\": \"Proposal Title\","
                  <> "\"abstract\": \"Proposal About\","
                  <> "\"motivation\": \"Proposal Motivation\","
                  <> "\"rationale\": \"Proposal Rationale\","
                  <> "\"dRepYesVotes\": 0,"
                  <> "\"dRepNoVotes\": 0,"
                  <> "\"dRepAbstainVotes\": 0,"
                  <> "\"poolYesVotes\": 0,"
                  <> "\"poolNoVotes\": 0,"
                  <> "\"poolAbstainVotes\": 0,"
                  <> "\"cCYesVotes\": 0,"
                  <> "\"cCNoVotes\": 0,"
                  <> "\"cCAbstainVotes\": 0,"
                  <> "\"prevGovActionIndex\": 0,"
                  <> "\"prevGovActionTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
                  <> "\"authors\": " <> exampleProposalAuthors
                  <> "}"

instance ToSchema Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "Value") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "Arbitrary JSON value"

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

data EnactedProposalDetailsResponse
  = EnactedProposalDetailsResponse
      { enactedProposalDetailsResponseId          :: Integer
      , enactedProposalDetailsResponseTxId        :: Integer
      , enactedProposalDetailsResponseIndex       :: Integer
      , enactedProposalDetailsResponseDescription :: Maybe Value
      , enactedProposalDetailsResponseHash        :: HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "enactedProposalDetailsResponse") ''EnactedProposalDetailsResponse

exampleEnactedProposalDetailsResponse :: Text
exampleEnactedProposalDetailsResponse = "{ \"id\": 123,"
                  <> "\"txId\": 456,"
                  <> "\"index\": 0,"
                  <> "\"description\": {\"key\": \"value\"},"
                  <> "\"hash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\"}"

instance ToSchema EnactedProposalDetailsResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
        ( fromAesonOptions $
            jsonOptions "enactedProposalDetailsResponse"
        )
        proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "Enacted Proposal Details Response"
          & example
            ?~ toJSON exampleEnactedProposalDetailsResponse

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
      { dRepInfoResponseIsScriptBased            :: Bool
      , dRepInfoResponseIsRegisteredAsDRep       :: Bool
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
      , dRepInfoResponsePaymentAddress           :: Maybe Text
      , dRepInfoResponseGivenName                :: Maybe Text
      , dRepInfoResponseObjectives               :: Maybe Text
      , dRepInfoResponseMotivations              :: Maybe Text
      , dRepInfoResponseQualifications            :: Maybe Text
      , dRepInfoResponseImageUrl                 :: Maybe Text
      , dRepInfoResponseImageHash                :: Maybe HexText
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "dRepInfoResponse") ''DRepInfoResponse

exampleDRepInfoResponse :: Text
exampleDRepInfoResponse =
    "{\"isScriptBased\": false,"
  <> "\"isRegisteredAsDRep\": true,"
  <> "\"wasRegisteredAsDRep\": false,"
  <> "\"isRegisteredAsSoleVoter\": true,"
  <> "\"wasRegisteredAsSoleVoter\": true,"
  <> "\"deposit\": 2000000,"
  <> "\"url\": \"https://drep.metadata.xyz\","
  <> "\"dataHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
  <> "\"votingPower\": 1000000,"
  <> "\"dRepRegisterTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"dRepRetireTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"soleVoterRegisterTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"soleVoterRetireTxHash\": \"47c14a128cd024f1b990c839d67720825921ad87ed875def42641ddd2169b39c\","
  <> "\"paymentAddress\": \"addr1qy49kr45ue0wq78d34dpg79syx3yekxryjadv9ykzczhjwm09pmyt6f6xvq5x9yah2vrxyg0np44ynm6n7hzafl2rqxs4v6nn3\","
  <> "\"givenName\": \"John Doe\","
  <> "\"objectives\": \"Objectives of the DRep\","
  <> "\"motivations\": \"Motivations of the DRep\","
  <> "\"qualifications\": \"Qualifications of the DRep\","
  <> "\"imageUrl\": \"https://drep.image.xyz\","
  <> "\"imageHash\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\"}"

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

data DRepVotingPowerListResponse
  = DRepVotingPowerListResponse
      { drepVotingPowerListResponseView        :: Text
      , drepVotingPowerListResponseHashRaw     :: HexText
      , drepVotingPowerListResponseVotingPower :: Integer
      , drepVotingPowerListResponseGivenName   :: Maybe Text
      }
  deriving (Generic, Show)

deriveJSON (jsonOptions "drepVotingPowerListResponse") ''DRepVotingPowerListResponse

exampleDRepVotingPowerListResponse :: Text
exampleDRepVotingPowerListResponse =
    "{\"view\": \"drep1qq5n7k0r0ff6lf4qvndw9t7vmdqa9y3q9qtjq879rrk9vcjcdy8a4xf92mqsajf9u3nrsh3r6zrp29kuydmfq45fz88qpzmjkc\","
  <> "\"hashRaw\": \"9af10e89979e51b8cdc827c963124a1ef4920d1253eef34a1d5cfe76438e3f11\","
  <> "\"votingPower\": 1000000,"
  <> "\"givenName\": \"John Doe\"}"

instance ToSchema DRepVotingPowerListResponse where
  declareNamedSchema proxy = do
    NamedSchema name_ schema_ <-
      genericDeclareNamedSchema
      ( fromAesonOptions $ jsonOptions "drepVotingPowerListResponse" )
      proxy
    return $
      NamedSchema name_ $
        schema_
          & description ?~ "DRep Voting Power List Response"
          & example
            ?~ toJSON exampleDRepVotingPowerListResponse

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

newtype GetTransactionStatusResponse
  = GetTransactionStatusResponse { getTransactionStatusResponse :: Maybe Value }
  deriving newtype (Show)

instance FromJSON GetTransactionStatusResponse where
  parseJSON = pure . GetTransactionStatusResponse . Just

instance ToJSON GetTransactionStatusResponse where
  toJSON (GetTransactionStatusResponse Nothing)       = Null
  toJSON (GetTransactionStatusResponse (Just status)) = toJSON status

exampleGetTransactionStatusResponse :: Text
exampleGetTransactionStatusResponse =
  "{ \"transactionConfirmed\": True, \"votingProcedure\": {\"vote\": \"yes\"}}"

instance ToSchema GetTransactionStatusResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetTransactionStatusResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Transaction status encoded as JSON"
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

newtype DRepReferences
  = DRepReferences { getDRepReferencesValue :: Value }
  deriving newtype (Show)

instance FromJSON DRepReferences where
  parseJSON v = return $ DRepReferences v

instance ToJSON DRepReferences where
  toJSON (DRepReferences d) = d

instance ToSchema DRepReferences where
    declareNamedSchema _ = pure $ NamedSchema (Just "DRepReferences") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A JSON value that can include nested objects and arrays"
        & example ?~ toJSON
            (Aeson.object
              [ "some_key" .= ("some value" :: String)
              , "nested_key" .= Aeson.object ["inner_key" .= (1 :: Int)]
              , "array_key" .= [1, 2, 3 :: Int]
              ])

data DRep
  = DRep
      { dRepIsScriptBased          :: Bool
      , dRepDrepId                 :: DRepHash
      , dRepView                   :: Text
      , dRepUrl                    :: Maybe Text
      , dRepMetadataHash           :: Maybe Text
      , dRepDeposit                :: Integer
      , dRepVotingPower            :: Maybe Integer
      , dRepStatus                 :: DRepStatus
      , dRepType                   :: DRepType
      , dRepLatestTxHash           :: Maybe HexText
      , dRepLatestRegistrationDate :: UTCTime
      , dRepMetadataError          :: Maybe Text
      , dRepPaymentAddress         :: Maybe Text
      , dRepGivenName              :: Maybe Text
      , dRepObjectives             :: Maybe Text
      , dRepMotivations            :: Maybe Text
      , dRepQualifications          :: Maybe Text
      , dRepImageUrl               :: Maybe Text
      , dRepImageHash              :: Maybe HexText
      , dRepIdentityReferences     :: Maybe DRepReferences
      , dRepLinkReferences         :: Maybe DRepReferences
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
  <> "\"paymentAddress\": \"addr1qy49kr45ue0wq78d34dpg79syx3yekxryjadv9ykzczhjwm09pmyt6f6xvq5x9yah2vrxyg0np44ynm6n7hzafl2rqxs4v6nn3\","
  <> "\"givenName\": \"John Doe\","
  <> "\"objectives\": \"Some Objectives\","
  <> "\"motivations\": \"Some Motivations\","
  <> "\"qualifications\": \"Some Qualifications\","
  <> "\"qualifications\": \"Some Qualifications\","
  <> "\"imageUrl\": \"https://image.url\","
  <> "\"imageHash\": \"9198b1b204273ba5c67a13310b5a806034160f6a063768297e161d9b759cad61\"}"

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
      { delegationResponseDRepHash          :: Maybe HexText
      , delegationResponseDRepView          :: Text
      , delegationResponseIsDRepScriptBased :: Bool
      , delegationResponseTxHash            :: HexText
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

data GetNetworkInfoResponse
  = GetNetworkInfoResponse
    { getNetworkInfoResponseCurrentTime                     :: UTCTime
    , getNetworkInfoResponseEpochNo                         :: Integer
    , getNetworkInfoResponseBlockNo                         :: Integer
    , getNetworkInfoResponseNetworkName                     :: Text
    }

deriveJSON (jsonOptions "getNetworkInfoResponse") ''GetNetworkInfoResponse

exampleGetNetworkInfoResponse :: Text
exampleGetNetworkInfoResponse =
  "{\"currentTime\": \"1970-01-01T00:00:00Z\","
  <> "\"currentEpoch\": 0,"
  <> "\"currentBlock\": 0,"
  <> "\"networkName\": \"Mainnet\"}"

instance ToSchema GetNetworkInfoResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetNetworkInfoResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetNetworkInfoResponse"
        & example
          ?~ toJSON exampleGetNetworkInfoResponse

data GetNetworkTotalStakeResponse
  = GetNetworkTotalStakeResponse
    { getNetworkTotalStakeResponseTotalStakeControlledByDReps   :: Integer
    , getNetworkTotalStakeResponseTotalStakeControlledBySPOs    :: Integer
    , getNetworkTotalStakeResponseAlwaysAbstainVotingPower      :: Integer
    , getNetworkTotalStakeResponseAlwaysNoConfidenceVotingPower  :: Integer
    }

deriveJSON (jsonOptions "getNetworkTotalStakeResponse") ''GetNetworkTotalStakeResponse

exampleGetNetworkTotalStakeResponse :: Text
exampleGetNetworkTotalStakeResponse =
  "{\"totalStakeControlledByDReps\": 0,"
  <> "\"totalStakeControlledBySPOs\": 0,"
  <> "\"alwaysAbstainVotingPower\": 0,"
  <> "\"alwaysNoConfidenceVotingPower\": 0}"

instance ToSchema GetNetworkTotalStakeResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetNetworkTotalStakeResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetNetworkTotalStakeResponse"
        & example
          ?~ toJSON exampleGetNetworkTotalStakeResponse

data GetNetworkMetricsResponse
  = GetNetworkMetricsResponse
      { getNetworkMetricsResponseUniqueDelegators                :: Integer
      , getNetworkMetricsResponseTotalDelegations                :: Integer
      , getNetworkMetricsResponseTotalGovernanceActions          :: Integer
      , getNetworkMetricsResponseTotalDRepVotes                  :: Integer
      , getNetworkMetricsResponseTotalRegisteredDReps            :: Integer
      , getNetworkMetricsResponseTotalDRepDistr                  :: Integer
      , getNetworkMetricsResponseTotalActiveDReps                :: Integer
      , getNetworkMetricsResponseTotalInactiveDReps              :: Integer
      , getNetworkMetricsResponseTotalActiveCIP119CompliantDReps :: Integer
      , getNetworkMetricsResponseTotalRegisteredDirectVoters     :: Integer
      , getNetworkMetricsResponseNoOfCommitteeMembers            :: Integer
      , getNetworkMetricsResponseQuorumNumerator                 :: Integer
      , getNetworkMetricsResponseQuorumDenominator               :: Integer
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
  <> "\"totalDRepDistr\": 0,"
  <> "\"totalActiveDReps\": 0,"
  <> "\"totalInactiveDReps\": 0,"
  <> "\"totalActiveCIP119CompliantDReps\": 0,"
  <> "\"totalRegisteredDirectVoters\": 0,"
  <> "\"networkName\": \"Mainnet\","
  <> "\"noOfCommitteeMembers\": 7,"
  <> "\"quorumNumerator\": 2,"
  <> "\"quorumDenominator\": 3}"

instance ToSchema GetNetworkMetricsResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetNetworkMetricsResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetNetworkMetricsResponse"
        & example
          ?~ toJSON exampleGetNetworkMetricsResponse

data GetAccountInfoResponse
  = GetAccountInfoResponse
      { getAccountInfoResponseId            :: Integer
      , getAccountInfoResponseView          :: Text
      , getAccountInfoResponseIsRegistered  :: Bool
      , getAccountInfoResponseIsScriptBased :: Bool
      }
  deriving (Generic, Show)
deriveJSON (jsonOptions "getAccountInfoResponse") ''GetAccountInfoResponse
exampleGetAccountInfoResponse :: Text
exampleGetAccountInfoResponse =
  "{\"stakeKey\": \"stake1u9\","
  <> " \"id\": \"1\","
  <> "\"view\": \"stake_test1uzapf83wydusjln97rqr7fen6vgrz5087yqdxm0akqdqkgstj2345\","
  <> "\"isRegistered\": false,"
  <> "\"isScriptBased\": false}"
instance ToSchema GetAccountInfoResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "GetAccountInfoResponse") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "GetAccountInfoResponse"
        & example
          ?~ toJSON exampleGetAccountInfoResponse
