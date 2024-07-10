{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module VVA.Types where

import           Control.Exception
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)

import           Data.Aeson                 (Value)
import qualified Data.Cache                 as Cache
import           Data.Has
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)

import           Database.PostgreSQL.Simple (Connection)

import           VVA.Cache
import           VVA.Config
import           Network.HTTP.Client (Manager)
import           Control.Concurrent.QSem

type App m = (MonadReader AppEnv m, MonadIO m, MonadFail m, MonadError AppError m)

data AppEnv
  = AppEnv
      { vvaConfig         :: VVAConfig
      , vvaCache          :: CacheEnv
      , vvaConnectionPool :: Pool Connection
      , vvaTlsManager     :: Manager
      , vvaMetadataQSem   :: QSem
      }

instance Has VVAConfig AppEnv where
  getter AppEnv {vvaConfig} = vvaConfig
  modifier f a@AppEnv {vvaConfig} = a {vvaConfig = f vvaConfig}

instance Has CacheEnv AppEnv where
  getter AppEnv {vvaCache} = vvaCache
  modifier f a@AppEnv {vvaCache} = a {vvaCache = f vvaCache}

instance Has (Pool Connection) AppEnv where
  getter AppEnv {vvaConnectionPool} = vvaConnectionPool
  modifier f a@AppEnv {vvaConnectionPool} = a {vvaConnectionPool = f vvaConnectionPool}

instance Has Manager AppEnv where
  getter AppEnv {vvaTlsManager} = vvaTlsManager
  modifier f a@AppEnv {vvaTlsManager} = a {vvaTlsManager = f vvaTlsManager}

instance Has QSem AppEnv where
  getter AppEnv {vvaMetadataQSem} = vvaMetadataQSem
  modifier f a@AppEnv {vvaMetadataQSem} = a {vvaMetadataQSem = f vvaMetadataQSem}


data AppError
  = ValidationError Text
  | NotFoundError Text
  | CriticalError Text
  | InternalError Text
  deriving (Show)

instance Exception AppError

data Vote
  = Vote
      { voteProposalId :: Integer
      , voteDrepId     :: Text
      , voteVote       :: Text
      , voteUrl        :: Maybe Text
      , voteDocHash    :: Maybe Text
      , voteEpochNo    :: Integer
      , voteDate       :: UTCTime
      , voteTxHash     :: Text
      }

data DRepInfo
  = DRepInfo
      { dRepInfoIsRegisteredAsDRep       :: Bool
      , dRepInfoWasRegisteredAsDRep      :: Bool
      , dRepInfoIsRegisteredAsSoleVoter  :: Bool
      , dRepInfoWasRegisteredAsSoleVoter :: Bool
      , dRepInfoDeposit                  :: Maybe Integer
      , dRepInfoUrl                      :: Maybe Text
      , dRepInfoDataHash                 :: Maybe Text
      , dRepInfoVotingPower              :: Maybe Integer
      , dRepInfoDRepRegisterTx           :: Maybe Text
      , dRepInfoDRepRetireTx             :: Maybe Text
      , dRepInfoSoleVoterRegisterTx      :: Maybe Text
      , dRepInfoSoleVoterRetireTx        :: Maybe Text
      }

data DRepStatus = Active | Inactive | Retired deriving (Eq, Ord)

data DRepType = DRep | SoleVoter deriving (Eq)

data DRepRegistration
  = DRepRegistration
      { dRepRegistrationDRepHash               :: Text
      , dRepRegistrationView                   :: Text
      , dRepRegistrationUrl                    :: Maybe Text
      , dRepRegistrationDataHash               :: Maybe Text
      , dRepRegistrationDeposit                :: Integer
      , dRepRegistrationVotingPower            :: Maybe Integer
      , dRepRegistrationStatus                 :: DRepStatus
      , dRepRegistrationType                   :: DRepType
      , dRepRegistrationLatestTxHash           :: Maybe Text
      , dRepRegistrationLatestRegistrationDate :: UTCTime
      }

data Proposal
  = Proposal
      { proposalId             :: Integer
      , proposalTxHash         :: Text
      , proposalIndex          :: Integer
      , proposalType           :: Text
      , proposalDetails        :: Maybe Value
      , proposalExpiryDate     :: Maybe UTCTime
      , proposalExpiryEpochNo  :: Maybe Integer
      , proposalCreatedDate    :: UTCTime
      , proposalCreatedEpochNo :: Integer
      , proposalUrl            :: Text
      , proposalDocHash        :: Text
      , proposalTitle          :: Maybe Text
      , proposalAbout          :: Maybe Text
      , proposalMotivation     :: Maybe Text
      , proposalRationale      :: Maybe Text
      , proposalMetadata       :: Maybe Value
      , proposalReferences     :: Maybe Value
      , proposalYesVotes       :: Integer
      , proposalNoVotes        :: Integer
      , proposalAbstainVotes   :: Integer
      }
  deriving (Show)

data TransactionStatus = TransactionConfirmed | TransactionUnconfirmed


data ProposalMetadata =
    ProposalMetadata
    { proposalMetadataAbstract :: Text
    , proposalMetadataMotivation :: Text
    , proposalMetadataRationale :: Text
    , proposalMetadataTitle :: Text
    , proposalMetadataReferences :: [Text]
    } deriving (Show)

data DRepMetadata =
    DRepMetadata
    { dRepMetadataBio :: Text
    , dRepMetadataDRepName :: Text
    , dRepMetadataEmail :: Text
    , dRepMetadataReferences :: [Text]
    } deriving (Show)

data MetadataValidationResult a =
    MetadataValidationResult
    { metadataValidationResultValid :: Bool
    , metadataValidationResultStatus :: Maybe Text
    , metadataValidationResultMetadata :: Maybe a
    } deriving (Show)




data CacheEnv
  = CacheEnv
      { proposalListCache                  :: Cache.Cache () [Proposal]
      , getProposalCache                   :: Cache.Cache (Text, Integer) Proposal
      , currentEpochCache                  :: Cache.Cache () (Maybe Value)
      , adaHolderVotingPowerCache          :: Cache.Cache Text Integer
      , adaHolderGetCurrentDelegationCache :: Cache.Cache Text (Maybe Delegation)
      , dRepGetVotesCache                  :: Cache.Cache Text ([Vote], [Proposal])
      , dRepInfoCache                      :: Cache.Cache Text DRepInfo
      , dRepVotingPowerCache               :: Cache.Cache Text Integer
      , dRepListCache                      :: Cache.Cache () [DRepRegistration]
      , networkMetricsCache                :: Cache.Cache () NetworkMetrics
      , proposalMetadataValidationCache            :: Cache.Cache (Text, Text) (MetadataValidationResult ProposalMetadata)
      , dRepMetadataValidationCache                :: Cache.Cache (Text, Text) (MetadataValidationResult DRepMetadata)
      }

data NetworkMetrics
  = NetworkMetrics
      { networkMetricsCurrentTime                   :: UTCTime
      , networkMetricsCurrentEpoch                  :: Integer
      , networkMetricsCurrentBlock                  :: Integer
      , networkMetricsUniqueDelegators              :: Integer
      , networkMetricsTotalDelegations              :: Integer
      , networkMetricsTotalGovernanceActions        :: Integer
      , networkMetricsTotalDRepVotes                :: Integer
      , networkMetricsTotalRegisteredDReps          :: Integer
      , networkMetricsAlwaysAbstainVotingPower      :: Integer
      , networkMetricsAlwaysNoConfidenceVotingPower :: Integer
      }

data Delegation
  = Delegation
      { delegationDRepHash :: Maybe Text
      , delegationDRepView :: Text
      , delegationTxHash   :: Text
      }


data MetadataValidationStatus
  = IncorrectFormat
  | IncorrectJSONLD
  | IncorrectHash
  | UrlNotFound


