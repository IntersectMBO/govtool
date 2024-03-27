{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module VVA.API where

import           Control.Exception    (throw)
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader

import           Data.Bool            (Bool)
import           Data.List            (sortOn)
import qualified Data.Map             as Map
import           Data.Maybe           (Maybe (Nothing), fromMaybe)
import           Data.Ord             (Down (..))
import           Data.Text            hiding (drop, elem, filter, length, map,
                                       null, take)
import qualified Data.Text            as Text

import           Numeric.Natural      (Natural)

import           Servant.API
import           Servant.Server

import           Text.Read            (readMaybe)

import qualified VVA.AdaHolder        as AdaHolder
import           VVA.API.Types
import           VVA.Cache            (cacheRequest)
import           VVA.Config
import qualified VVA.DRep             as DRep
import qualified VVA.Epoch            as Epoch
import           VVA.Network          as Network
import qualified VVA.Proposal         as Proposal
import qualified VVA.Transaction      as Transaction
import qualified VVA.Types            as Types
import           VVA.Types            (App, AppEnv (..),
                                       AppError (CriticalError, ValidationError),
                                       CacheEnv (..))

type VVAApi =
         "drep" :> "list" :> QueryParam "drepView" Text :> Get '[JSON] [DRep]
    :<|> "drep" :> "get-voting-power" :> Capture "drepId" HexText :> Get '[JSON] Integer
    :<|> "drep" :> "getVotes" :> Capture "drepId" HexText :> QueryParams "type" GovernanceActionType :> QueryParam "sort" GovernanceActionSortMode :> Get '[JSON] [VoteResponse]
    :<|> "drep" :> "info" :> Capture "drepId" HexText :> Get '[JSON] DRepInfoResponse
    :<|> "ada-holder" :> "get-current-delegation" :> Capture "stakeKey" HexText :> Get '[JSON] (Maybe HexText)
    :<|> "ada-holder" :> "get-voting-power" :> Capture "stakeKey" HexText :> Get '[JSON] Integer
    :<|> "proposal" :> "list"
                    :> QueryParams "type" GovernanceActionType
                    :> QueryParam "sort" GovernanceActionSortMode
                    :> QueryParam "page" Natural
                    :> QueryParam "pageSize" Natural
                    :> QueryParam "drepId" HexText
                    :> QueryParam "search" Text
                    :> Get '[JSON] ListProposalsResponse
    :<|> "proposal" :> "get" :> Capture "proposalId" GovActionId :> QueryParam "drepId" HexText :> Get '[JSON] GetProposalResponse
    :<|> "epoch" :> "params" :> Get '[JSON] GetCurrentEpochParamsResponse
    :<|> "transaction" :> "status" :> Capture "transactionId" HexText :> Get '[JSON] GetTransactionStatusResponse
    :<|> "throw500" :> Get '[JSON] ()
    :<|> "network" :> "metrics" :> Get '[JSON] GetNetworkMetricsResponse

server :: App m => ServerT VVAApi m
server = drepList
    :<|> getVotingPower
    :<|> getVotes
    :<|> drepInfo
    :<|> getCurrentDelegation
    :<|> getStakeKeyVotingPower
    :<|> listProposals
    :<|> getProposal
    :<|> getCurrentEpochParams
    :<|> getTransactionStatus
    :<|> throw500
    :<|> getNetworkMetrics


mapDRepType :: Types.DRepType -> DRepType
mapDRepType Types.DRep      = NormalDRep
mapDRepType Types.SoleVoter = SoleVoter

mapDRepStatus :: Types.DRepStatus -> DRepStatus
mapDRepStatus Types.Retired  = Retired
mapDRepStatus Types.Active   = Active
mapDRepStatus Types.Inactive = Inactive

drepRegistrationToDrep :: Types.DRepRegistration -> DRep
drepRegistrationToDrep Types.DRepRegistration {..} =
  DRep
    { dRepDrepId = DRepHash dRepRegistrationDRepHash,
      dRepView = dRepRegistrationView,
      dRepUrl = dRepRegistrationUrl,
      dRepMetadataHash = dRepRegistrationDataHash,
      dRepDeposit = dRepRegistrationDeposit,
      dRepVotingPower = dRepRegistrationVotingPower,
      dRepStatus = mapDRepStatus dRepRegistrationStatus,
      dRepType = mapDRepType dRepRegistrationType
    }

drepList :: App m => Maybe Text -> m [DRep]
drepList mDRepView = do
  CacheEnv {dRepListCache} <- asks vvaCache
  dreps <- cacheRequest dRepListCache () DRep.listDReps
  let filtered = flip filter dreps $ \Types.DRepRegistration {..} ->
        case (dRepRegistrationType, mDRepView) of
          (Types.SoleVoter, Just x) -> x == dRepRegistrationView
          (Types.DRep, Just x)      -> x `isInfixOf` dRepRegistrationView
          (Types.DRep, Nothing)     -> True
          _                         -> False
  return $ map drepRegistrationToDrep filtered

getVotingPower :: App m => HexText -> m Integer
getVotingPower (unHexText -> dRepId) = do
  CacheEnv {dRepVotingPowerCache} <- asks vvaCache
  cacheRequest dRepVotingPowerCache dRepId $ DRep.getVotingPower dRepId


proposalToResponse :: Types.Proposal -> ProposalResponse
proposalToResponse Types.Proposal {..} =
  ProposalResponse
  { proposalResponseId = pack $ show proposalId,
    proposalResponseTxHash = HexText proposalTxHash,
    proposalResponseIndex = proposalIndex,
    proposalResponseType = fromMaybe InfoAction $ readMaybe $ unpack proposalType,
    proposalResponseDetails = GovernanceActionDetails <$> proposalDetails,
    proposalResponseExpiryDate = proposalExpiryDate,
    proposalResponseExpiryEpochNo = proposalExpiryEpochNo,
    proposalResponseCreatedDate = proposalCreatedDate,
    proposalResponseCreatedEpochNo = proposalCreatedEpochNo,
    proposalResponseUrl = proposalUrl,
    proposalResponseMetadataHash = HexText proposalDocHash,
    proposalResponseTitle = proposalTitle,
    proposalResponseAbout = proposalAbout,
    proposalResponseMotivation = proposalMotivaiton,
    proposalResponseRationale = proposalRationale,
    proposalResponseMetadata = GovernanceActionMetadata <$> proposalMetadata,
    proposalResponseYesVotes = proposalYesVotes,
    proposalResponseNoVotes = proposalNoVotes,
    proposalResponseAbstainVotes = proposalAbstainVotes
  }

voteToResponse :: Types.Vote -> VoteParams
voteToResponse Types.Vote {..} =
  VoteParams
  { voteParamsProposalId = pack $ show voteProposalId,
    voteParamsDrepId = HexText voteDrepId,
    voteParamsVote = voteVote,
    voteParamsUrl = voteUrl,
    voteParamsMetadataHash = HexText <$> voteDocHash,
    voteParamsEpochNo = voteEpochNo,
    voteParamsDate = voteDate
  }


mapSortAndFilterProposals
  :: [GovernanceActionType]
  -> Maybe GovernanceActionSortMode
  -> [Types.Proposal]
  -> [ProposalResponse]
mapSortAndFilterProposals selectedTypes sortMode proposals =
  let mappedProposals =
        map
          proposalToResponse
          proposals
      filteredProposals =
        if null selectedTypes
          then mappedProposals
          else
            filter
              ( \ProposalResponse {proposalResponseType} ->
                  proposalResponseType `elem` selectedTypes
              )
              mappedProposals
      sortedProposals = case sortMode of
        Nothing              -> filteredProposals
        Just NewestCreated   -> sortOn (Down . proposalResponseCreatedDate) filteredProposals
        Just SoonestToExpire -> sortOn proposalResponseExpiryDate filteredProposals
        Just MostYesVotes    -> sortOn (Down . proposalResponseYesVotes) filteredProposals
  in sortedProposals

getVotes :: App m => HexText -> [GovernanceActionType] -> Maybe GovernanceActionSortMode -> m [VoteResponse]
getVotes (unHexText -> dRepId) selectedTypes sortMode = do
  CacheEnv {dRepGetVotesCache} <- asks vvaCache
  (votes, proposals) <- cacheRequest dRepGetVotesCache dRepId $ DRep.getVotes dRepId []
  let voteMap = Map.fromList $ map (\vote@Types.Vote {..} -> (voteProposalId, vote)) votes
  let processedProposals = mapSortAndFilterProposals selectedTypes sortMode proposals
  return $
    [ VoteResponse
      { voteResponseVote = voteToResponse (voteMap Map.! read (unpack proposalResponseId))
      , voteResponseProposal = proposalResponse
      }
    | proposalResponse@ProposalResponse{proposalResponseId} <- processedProposals
    ]

drepInfo :: App m => HexText -> m DRepInfoResponse
drepInfo (unHexText -> dRepId) = do
  CacheEnv {dRepInfoCache} <- asks vvaCache
  Types.DRepInfo {..} <- cacheRequest dRepInfoCache dRepId $ DRep.getDRepInfo dRepId
  return $ DRepInfoResponse
    { dRepInfoResponseIsRegisteredAsDRep = dRepInfoIsRegisteredAsDRep
    , dRepInfoResponseWasRegisteredAsDRep = dRepInfoWasRegisteredAsDRep
    , dRepInfoResponseIsRegisteredAsSoleVoter = dRepInfoIsRegisteredAsSoleVoter
    , dRepInfoResponseWasRegisteredAsSoleVoter = dRepInfoWasRegisteredAsSoleVoter
    , dRepInfoResponseDeposit = dRepInfoDeposit
    , dRepInfoResponseUrl = dRepInfoUrl
    , dRepInfoResponseDataHash = HexText <$> dRepInfoDataHash
    , dRepInfoResponseVotingPower = dRepInfoVotingPower
    }

getCurrentDelegation :: App m => HexText -> m (Maybe HexText)
getCurrentDelegation (unHexText -> stakeKey) = do
  CacheEnv {adaHolderGetCurrentDelegationCache} <- asks vvaCache
  result <- cacheRequest adaHolderGetCurrentDelegationCache stakeKey $ AdaHolder.getCurrentDelegation stakeKey
  return $ HexText <$> result

getStakeKeyVotingPower :: App m => HexText -> m Integer
getStakeKeyVotingPower (unHexText -> stakeKey) = do
  CacheEnv {adaHolderVotingPowerCache} <- asks vvaCache
  cacheRequest adaHolderVotingPowerCache stakeKey $ AdaHolder.getStakeKeyVotingPower stakeKey


listProposals
  :: App m
  => [GovernanceActionType]
  -> Maybe GovernanceActionSortMode
  -> Maybe Natural
  -> Maybe Natural
  -> Maybe HexText
  -> Maybe Text
  -> m ListProposalsResponse
listProposals selectedTypes sortMode mPage mPageSize mDrepRaw mSearchQuery = do
  let page = (fromIntegral $ fromMaybe 0 mPage) :: Int
      pageSize = (fromIntegral $ fromMaybe 10 mPageSize) :: Int

  -- proposals that the provided Drep has already voted on should be filtered out
  proposalsToRemove <- case mDrepRaw of
    Nothing -> return []
    Just drepId ->
      map (voteParamsProposalId . voteResponseVote)
        <$> getVotes drepId [] Nothing



  let filterF ProposalResponse{..} = case Text.toLower <$> mSearchQuery of
        Nothing -> True
        Just searchQuery -> fromMaybe False $ do
          title <- Text.toLower <$> proposalResponseTitle
          about <- Text.toLower <$> proposalResponseAbout
          motivation <- Text.toLower <$> proposalResponseMotivation
          rationale <- Text.toLower <$> proposalResponseRationale

          let result = searchQuery `isInfixOf` title
                      || searchQuery `isInfixOf` about
                      || searchQuery `isInfixOf` motivation
                      || searchQuery `isInfixOf` rationale

          pure result

  CacheEnv {proposalListCache} <- asks vvaCache
  mappedAndSortedProposals <-
    filter
      ( \p@ProposalResponse {proposalResponseId} ->
          proposalResponseId `notElem` proposalsToRemove
          && filterF p
      ) . mapSortAndFilterProposals selectedTypes sortMode <$> cacheRequest proposalListCache () Proposal.listProposals

  let total = length mappedAndSortedProposals :: Int

  let elements = take pageSize $ drop (page * pageSize) mappedAndSortedProposals

  return $ ListProposalsResponse
    { listProposalsResponsePage = fromIntegral page
    , listProposalsResponsePageSize = fromIntegral pageSize
    , listProposalsResponseTotal = fromIntegral total
    , listProposalsResponseElements = elements
    }

getProposal :: App m => GovActionId -> Maybe HexText -> m GetProposalResponse
getProposal g@(GovActionId govActionTxHash govActionIndex) mDrepId' = do
  let mDrepId = unHexText <$> mDrepId'
  CacheEnv {getProposalCache} <- asks vvaCache
  proposalResponse <- proposalToResponse <$> cacheRequest getProposalCache (unHexText govActionTxHash, govActionIndex) (Proposal.getProposal (unHexText govActionTxHash) govActionIndex)
  voteResponse <- case mDrepId of
    Nothing -> return Nothing
    Just drepId -> do
      (votes, _) <- DRep.getVotes drepId [Text.pack (show g)]
      case votes of
        [vote] ->
          return $ Just $ voteToResponse vote
        [] -> return Nothing
        _ -> throwError $ CriticalError ("More than 1 vote of DRep " <> drepId <> " for proposal " <> Text.pack (show g) <> ". This should never happen")

  return $ GetProposalResponse
    { getProposalResponseProposal = proposalResponse
    , getProposalResponseVote = voteResponse
    }

getCurrentEpochParams :: App m => m GetCurrentEpochParamsResponse
getCurrentEpochParams = do
  CacheEnv {currentEpochCache} <- asks vvaCache
  GetCurrentEpochParamsResponse <$> cacheRequest currentEpochCache () Epoch.getCurrentEpochParams

getTransactionStatus :: App m => HexText -> m GetTransactionStatusResponse
getTransactionStatus (unHexText -> transactionId) = do
  x <- Transaction.getTransactionStatus transactionId
  case x of
    Types.TransactionConfirmed   -> return $ GetTransactionStatusResponse True
    Types.TransactionUnconfirmed -> return $ GetTransactionStatusResponse False

throw500 :: App m => m ()
throw500 = throwError $ CriticalError "intentional system break for testing purposes"

getNetworkMetrics :: App m => m GetNetworkMetricsResponse
getNetworkMetrics = do
  CacheEnv {networkMetricsCache} <- asks vvaCache
  Types.NetworkMetrics {..} <- Network.networkMetrics
  return $ GetNetworkMetricsResponse
    { getNetworkMetricsResponseCurrentTime = networkMetricsCurrentTime
    , getNetworkMetricsResponseCurrentEpoch = networkMetricsCurrentEpoch
    , getNetworkMetricsResponseCurrentBlock = networkMetricsCurrentBlock
    , getNetworkMetricsResponseUniqueDelegators = networkMetricsUniqueDelegators
    , getNetworkMetricsResponseTotalDelegations = networkMetricsTotalDelegations
    , getNetworkMetricsResponseTotalGovernanceActions = networkMetricsTotalGovernanceActions
    , getNetworkMetricsResponseTotalDRepVotes = networkMetricsTotalDRepVotes
    , getNetworkMetricsResponseTotalRegisteredDReps = networkMetricsTotalRegisteredDReps
    , getNetworkMetricsResponseAlwaysAbstainVotingPower = networkMetricsAlwaysAbstainVotingPower
    , getNetworkMetricsResponseAlwaysNoConfidenceVotingPower = networkMetricsAlwaysNoConfidenceVotingPower
    }
