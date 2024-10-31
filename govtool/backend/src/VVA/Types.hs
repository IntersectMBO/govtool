{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}

module VVA.Types where

import           Control.Concurrent.QSem
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
import           Data.Time                  (UTCTime, LocalTime)
import           Data.Scientific

import           Database.PostgreSQL.Simple (Connection)
import           Database.PostgreSQL.Simple.FromRow

import           VVA.Cache
import           VVA.Config

type App m = (MonadReader AppEnv m, MonadIO m, MonadFail m, MonadError AppError m)

data AppEnv
  = AppEnv
      { vvaConfig         :: VVAConfig
      , vvaCache          :: CacheEnv
      , vvaConnectionPool :: Pool Connection
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
      { dRepInfoIsScriptBased            :: Bool
      , dRepInfoIsRegisteredAsDRep       :: Bool
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
      , dRepInfoPaymentAddress           :: Maybe Text
      , dRepInfoGivenName                :: Maybe Text
      , dRepInfoObjectives               :: Maybe Text
      , dRepInfoMotivations              :: Maybe Text
      , dRepInfoQualifications           :: Maybe Text
      , dRepInfoImageUrl                 :: Maybe Text
      , dRepInfoImageHash                :: Maybe Text
      }

data DRepStatus = Active | Inactive | Retired deriving (Eq, Ord)

data DRepType = DRep | SoleVoter deriving (Eq)

data DRepRegistration
  = DRepRegistration
      { dRepRegistrationDRepHash               :: Text
      , dRepRegistrationView                   :: Text
      , dRepRegistrationIsScriptBased          :: Bool
      , dRepRegistrationUrl                    :: Maybe Text
      , dRepRegistrationDataHash               :: Maybe Text
      , dRepRegistrationDeposit                :: Integer
      , dRepRegistrationVotingPower            :: Maybe Integer
      , dRepRegistrationStatus                 :: DRepStatus
      , dRepRegistrationType                   :: DRepType
      , dRepRegistrationLatestTxHash           :: Maybe Text
      , dRepRegistrationLatestRegistrationDate :: UTCTime
      , dRepRegistrationMetadataError          :: Maybe Text
      , dRepRegistrationPaymentAddress         :: Maybe Text
      , dRepRegistrationGivenName              :: Maybe Text
      , dRepRegistrationObjectives             :: Maybe Text
      , dRepRegistrationMotivations            :: Maybe Text
      , dRepRegistrationQualifications         :: Maybe Text
      , dRepRegistrationImageUrl               :: Maybe Text
      , dRepRegistrationImageHash              :: Maybe Text
      }

data Proposal
  = Proposal
      { proposalId                  :: Integer
      , proposalTxHash              :: Text
      , proposalIndex               :: Integer
      , proposalType                :: Text
      , proposalDetails             :: Maybe Value
      , proposalExpiryDate          :: Maybe LocalTime
      , proposalExpiryEpochNo       :: Maybe Integer
      , proposalCreatedDate         :: LocalTime
      , proposalCreatedEpochNo      :: Integer
      , proposalUrl                 :: Text
      , proposalDocHash             :: Text
      , proposalProtocolParams      :: Maybe Value
      , proposalTitle               :: Maybe Text
      , proposalAbstract            :: Maybe Text
      , proposalMotivation          :: Maybe Text
      , proposalRationale           :: Maybe Text
      , proposalDRepYesVotes        :: Integer
      , proposalDRepNoVotes         :: Integer
      , proposalDRepAbstainVotes    :: Integer
      , proposalPoolYesVotes        :: Integer
      , proposalPoolNoVotes         :: Integer
      , proposalPoolAbstainVotes    :: Integer
      , proposalCcYesVotes          :: Integer
      , proposalCcNoVotes           :: Integer
      , proposalCcAbstainVotes      :: Integer
      , proposalPrevGovActionIndex  :: Maybe Integer
      , proposalPrevGovActionTxHash :: Maybe Text
      }
  deriving (Show)

instance FromRow Proposal where
  fromRow =
    Proposal
      <$> field
      <*> field
      <*> (floor @Scientific <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> (floor @Scientific <$> field)
      <*> field
      <*> field
      
data TransactionStatus = TransactionConfirmed | TransactionUnconfirmed

data CacheEnv
  = CacheEnv
      { proposalListCache :: Cache.Cache () [Proposal]
      , getProposalCache :: Cache.Cache (Text, Integer) Proposal
      , currentEpochCache :: Cache.Cache () (Maybe Value)
      , adaHolderVotingPowerCache :: Cache.Cache Text Integer
      , adaHolderGetCurrentDelegationCache :: Cache.Cache Text (Maybe Delegation)
      , dRepGetVotesCache :: Cache.Cache Text ([Vote], [Proposal])
      , dRepInfoCache :: Cache.Cache Text DRepInfo
      , dRepVotingPowerCache :: Cache.Cache Text Integer
      , dRepListCache :: Cache.Cache Text [DRepRegistration]
      , networkMetricsCache :: Cache.Cache () NetworkMetrics
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
      , networkMetricsNetworkName                   :: Text
      }

data Delegation
  = Delegation
      { delegationDRepHash          :: Maybe Text
      , delegationDRepView          :: Text
      , delegationIsDRepScriptBased :: Bool
      , delegationTxHash            :: Text
      }
