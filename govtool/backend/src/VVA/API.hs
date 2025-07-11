{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module VVA.API where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (throw, throwIO)
import           Control.Monad.Except     (runExceptT, throwError)
import           Control.Monad.Reader

import           Data.Aeson               (Value(..), Array, decode, encode, ToJSON, toJSON)
import           Data.Bool                (Bool)
import           Data.List                (sortOn, sort)
import qualified Data.Map                 as Map
import           Data.Maybe               (Maybe (Nothing), catMaybes, fromMaybe, mapMaybe)
import           Data.Ord                 (Down (..))
import           Data.Text                hiding (any, drop, elem, filter, length, map, null, take)
import qualified Data.Text                as Text
import qualified Data.Vector as V
import           Data.Time.LocalTime      (TimeZone, getCurrentTimeZone)


import           Numeric.Natural          (Natural)

import           Servant.API
import           Servant.Server
import           System.Random            (randomRIO)

import           Text.Read                (readMaybe)

import qualified VVA.AdaHolder            as AdaHolder
import           VVA.API.Types
import           VVA.Cache                (cacheRequest)
import           VVA.Config
import qualified VVA.DRep                 as DRep
import qualified VVA.Epoch                as Epoch
import           VVA.Network              as Network
import           VVA.Account              as Account
import qualified VVA.Proposal             as Proposal
import qualified VVA.Transaction          as Transaction
import qualified VVA.Types                as Types
import           VVA.Types                (App, AppEnv (..),
                                           AppError (CriticalError, InternalError, ValidationError),
                                           CacheEnv (..))
import Data.Time (TimeZone, localTimeToUTC)

type VVAApi =
         "drep" :> "list"
                :> QueryParam "search" Text
                :> QueryParams "status" DRepStatus
                :> QueryParam "sort" DRepSortMode
                :> QueryParam "page" Natural
                :> QueryParam "pageSize" Natural
                :> Get '[JSON] ListDRepsResponse
    :<|> "drep" :> "get-voting-power" :> Capture "drepId" HexText :> Get '[JSON] Integer
    :<|> "drep" :> "getVotes"
                :> Capture "drepId" HexText
                :> QueryParams "type" GovernanceActionType
                :> QueryParam "sort" GovernanceActionSortMode
                :> QueryParam "search" Text
                :> Get '[JSON] [VoteResponse]
    :<|> "drep" :> "info" :> Capture "drepId" HexText :> Get '[JSON] DRepInfoResponse
    :<|> "drep" :> "voting-power-list"
                :> QueryParams "identifiers" Text
                :> Get '[JSON] [DRepVotingPowerListResponse]
    :<|> "ada-holder" :> "get-current-delegation" :> Capture "stakeKey" HexText :> Get '[JSON] (Maybe DelegationResponse)
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
    :<|> "proposal" :> "enacted-details" :> QueryParam "type" GovernanceActionType :> Get '[JSON] (Maybe EnactedProposalDetailsResponse)
    :<|> "epoch" :> "params" :> Get '[JSON] GetCurrentEpochParamsResponse
    :<|> "transaction" :> "status" :> Capture "transactionId" HexText :> Get '[JSON] GetTransactionStatusResponse
    :<|> "throw500" :> Get '[JSON] ()
    :<|> "network" :> "metrics" :> Get '[JSON] GetNetworkMetricsResponse
    :<|> "network" :> "info" :> Get '[JSON] GetNetworkInfoResponse
    :<|> "network" :> "total-stake" :> Get '[JSON] GetNetworkTotalStakeResponse
    :<|> "account" :> Capture "stakeKey" HexText :> Get '[JSON] GetAccountInfoResponse

server :: App m => ServerT VVAApi m
server = drepList
    :<|> getVotingPower
    :<|> getVotes
    :<|> drepInfo
    :<|> drepVotingPowerList
    :<|> getCurrentDelegation
    :<|> getStakeKeyVotingPower
    :<|> listProposals
    :<|> getProposal
    :<|> getEnactedProposalDetails
    :<|> getCurrentEpochParams
    :<|> getTransactionStatus
    :<|> throw500
    :<|> getNetworkMetrics
    :<|> getNetworkInfo
    :<|> getNetworkTotalStake
    :<|> getAccountInfo

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
    { dRepIsScriptBased = dRepRegistrationIsScriptBased,
      dRepDrepId = DRepHash dRepRegistrationDRepHash,
      dRepView = dRepRegistrationView,
      dRepUrl = dRepRegistrationUrl,
      dRepMetadataHash = dRepRegistrationDataHash,
      dRepDeposit = dRepRegistrationDeposit,
      dRepVotingPower = dRepRegistrationVotingPower,
      dRepStatus = mapDRepStatus dRepRegistrationStatus,
      dRepType = mapDRepType dRepRegistrationType,
      dRepLatestTxHash = HexText <$> dRepRegistrationLatestTxHash,
      dRepLatestRegistrationDate = dRepRegistrationLatestRegistrationDate,
      dRepMetadataError = dRepRegistrationMetadataError,
      dRepPaymentAddress = dRepRegistrationPaymentAddress,
      dRepGivenName = dRepRegistrationGivenName,
      dRepObjectives = dRepRegistrationObjectives,
      dRepMotivations = dRepRegistrationMotivations,
      dRepQualifications = dRepRegistrationQualifications,
      dRepImageUrl = dRepRegistrationImageUrl,
      dRepImageHash = HexText <$> dRepRegistrationImageHash,
      dRepIdentityReferences = DRepReferences <$> dRepRegistrationIdentityReferences,
      dRepLinkReferences = DRepReferences <$> dRepRegistrationLinkReferences
    }

delegationToResponse :: Types.Delegation -> DelegationResponse
delegationToResponse Types.Delegation {..} =
  DelegationResponse
    { delegationResponseDRepHash = HexText <$> delegationDRepHash,
      delegationResponseDRepView = delegationDRepView,
      delegationResponseIsDRepScriptBased = delegationIsDRepScriptBased,
      delegationResponseTxHash = HexText delegationTxHash
    }


drepList :: App m => Maybe Text -> [DRepStatus] -> Maybe DRepSortMode -> Maybe Natural -> Maybe Natural -> m ListDRepsResponse
drepList mSearchQuery statuses mSortMode mPage mPageSize = do
  CacheEnv {dRepListCache} <- asks vvaCache
  dreps <- cacheRequest dRepListCache (fromMaybe "" mSearchQuery) (DRep.listDReps mSearchQuery)

  let filterDRepsByQuery = case mSearchQuery of
        Nothing -> filter $ \Types.DRepRegistration {..} ->
          dRepRegistrationType /= Types.SoleVoter
        Just query -> filter $ \Types.DRepRegistration {..} ->
          let searchLower = Text.toLower query
              viewLower = Text.toLower dRepRegistrationView
              hashLower = Text.toLower dRepRegistrationDRepHash
          in case dRepRegistrationType of
              Types.SoleVoter -> 
                searchLower == viewLower || searchLower == hashLower
              Types.DRep -> 
                True


  let filterDRepsByStatus = case statuses of
        [] -> id
        _  -> filter $ \Types.DRepRegistration {..} ->
          mapDRepStatus dRepRegistrationStatus `elem` statuses

  randomizedOrderList <- mapM (\_ -> randomRIO (0, 1 :: Double)) dreps

  let sortDReps = case mSortMode of
        Nothing -> id
        Just Random -> fmap snd . sortOn fst . Prelude.zip randomizedOrderList
        Just VotingPower -> sortOn $ \Types.DRepRegistration {..} ->
          Down dRepRegistrationVotingPower
        Just RegistrationDate -> sortOn $ \Types.DRepRegistration {..} ->
          Down dRepRegistrationLatestRegistrationDate
        Just Status -> sortOn $ \Types.DRepRegistration {..} ->
          dRepRegistrationStatus

  appEnv <- ask

  allValidDReps <- liftIO $ mapConcurrently
    (\d@Types.DRepRegistration{..} -> do
        let drep = drepRegistrationToDrep d
        return drep)
    $ sortDReps $ filterDRepsByQuery $ filterDRepsByStatus dreps

  let page = (fromIntegral $ fromMaybe 0 mPage) :: Int
      pageSize = (fromIntegral $ fromMaybe 10 mPageSize) :: Int

      total = length allValidDReps :: Int

  let elements = take pageSize $ drop (page * pageSize) allValidDReps
  return $ ListDRepsResponse
    { listDRepsResponsePage = fromIntegral page
    , listDRepsResponsePageSize = fromIntegral pageSize
    , listDRepsResponseTotal = fromIntegral total
    , listDRepsResponseElements = elements
    }


getVotingPower :: App m => HexText -> m Integer
getVotingPower (unHexText -> dRepId) = do
  CacheEnv {dRepVotingPowerCache} <- asks vvaCache
  cacheRequest dRepVotingPowerCache dRepId $ DRep.getVotingPower dRepId

proposalToResponse :: TimeZone -> Types.Proposal -> ProposalResponse
proposalToResponse timeZone Types.Proposal {..} =
  ProposalResponse
  { proposalResponseId = pack $ show proposalId,
    proposalResponseTxHash = HexText proposalTxHash,
    proposalResponseIndex = proposalIndex,
    proposalResponseType = fromMaybe InfoAction $ readMaybe $ unpack proposalType,
    proposalResponseDetails = GovernanceActionDetails <$> proposalDetails,
    proposalResponseExpiryDate = localTimeToUTC timeZone <$> proposalExpiryDate,
    proposalResponseExpiryEpochNo = proposalExpiryEpochNo,
    proposalResponseCreatedDate = localTimeToUTC timeZone proposalCreatedDate,
    proposalResponseCreatedEpochNo = proposalCreatedEpochNo,
    proposalResponseUrl = proposalUrl,
    proposalResponseMetadataHash = HexText proposalDocHash,
    proposalResponseProtocolParams = ProtocolParams <$> proposalProtocolParams,
    proposalResponseTitle =  proposalTitle,
    proposalResponseAbstract =  proposalAbstract,
    proposalResponseMotivation =  proposalMotivation,
    proposalResponseRationale = proposalRationale,
    proposalResponseDRepYesVotes = proposalDRepYesVotes,
    proposalResponseDRepNoVotes = proposalDRepNoVotes,
    proposalResponseDRepAbstainVotes = proposalDRepAbstainVotes,
    proposalResponsePoolYesVotes = proposalPoolYesVotes,
    proposalResponsePoolNoVotes = proposalPoolNoVotes,
    proposalResponsePoolAbstainVotes = proposalPoolAbstainVotes,
    proposalResponseCcYesVotes = proposalCcYesVotes,
    proposalResponseCcNoVotes = proposalCcNoVotes,
    proposalResponseCcAbstainVotes = proposalCcAbstainVotes,
    proposalResponsePrevGovActionIndex = proposalPrevGovActionIndex,
    proposalResponsePrevGovActionTxHash = HexText <$> proposalPrevGovActionTxHash,
    proposalResponseJson = proposalJson,
    proposalResponseAuthors = ProposalAuthors <$> proposalAuthors
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
    voteParamsDate = voteDate,
    voteParamsTxHash = HexText voteTxHash
  }

mapSortAndFilterProposals
  :: App m
  => [GovernanceActionType]
  -> Maybe GovernanceActionSortMode
  -> [Types.Proposal]
  -> m [ProposalResponse]
mapSortAndFilterProposals selectedTypes sortMode proposals = do
  timeZone <- liftIO getCurrentTimeZone

  let mappedProposals = map (proposalToResponse timeZone) proposals
  let filteredProposals =
        if null selectedTypes
          then mappedProposals
          else
            filter
              ( \ProposalResponse {proposalResponseType} ->
                  proposalResponseType `elem` selectedTypes
              )
              mappedProposals

  let totalYesVotes (ProposalResponse{..}) = proposalResponseDRepYesVotes + proposalResponsePoolYesVotes + proposalResponseCcYesVotes

  let sortedProposals = case sortMode of
        Nothing              -> filteredProposals
        Just NewestCreated   -> sortOn (Down . proposalResponseCreatedDate) filteredProposals
        Just SoonestToExpire -> sortOn proposalResponseExpiryDate filteredProposals
        Just MostYesVotes    -> sortOn (Down . totalYesVotes) filteredProposals
  return sortedProposals

getVotes :: App m => HexText -> [GovernanceActionType] -> Maybe GovernanceActionSortMode -> Maybe Text -> m [VoteResponse]
getVotes (unHexText -> dRepId) selectedTypes sortMode mSearch = do
  CacheEnv {dRepGetVotesCache} <- asks vvaCache
  (votes, proposals) <- cacheRequest dRepGetVotesCache dRepId $ DRep.getVotes dRepId []
  
  let voteMapByTxHash = Map.fromList $ 
        map (\vote -> (pack $ Prelude.takeWhile (/= '#') (unpack $ Types.voteGovActionId vote), vote)) votes
  
  processedProposals <- filter (isProposalSearchedFor mSearch) <$> 
                        mapSortAndFilterProposals selectedTypes sortMode proposals
  
  return $
    [ VoteResponse
      { voteResponseVote = voteToResponse vote
      , voteResponseProposal = proposalResponse
      }
    | proposalResponse <- processedProposals
    , let txHash = unHexText (proposalResponseTxHash proposalResponse)
    , Just vote <- [Map.lookup txHash voteMapByTxHash]
    ]

drepInfo :: App m => HexText -> m DRepInfoResponse
drepInfo (unHexText -> dRepId) = do
  CacheEnv {dRepInfoCache} <- asks vvaCache
  Types.DRepInfo {..} <- cacheRequest dRepInfoCache dRepId $ DRep.getDRepInfo dRepId
  return $ DRepInfoResponse
    { dRepInfoResponseIsScriptBased = dRepInfoIsScriptBased
    , dRepInfoResponseIsRegisteredAsDRep = dRepInfoIsRegisteredAsDRep
    , dRepInfoResponseWasRegisteredAsDRep = dRepInfoWasRegisteredAsDRep
    , dRepInfoResponseIsRegisteredAsSoleVoter = dRepInfoIsRegisteredAsSoleVoter
    , dRepInfoResponseWasRegisteredAsSoleVoter = dRepInfoWasRegisteredAsSoleVoter
    , dRepInfoResponseDeposit = dRepInfoDeposit
    , dRepInfoResponseUrl = dRepInfoUrl
    , dRepInfoResponseDataHash = HexText <$> dRepInfoDataHash
    , dRepInfoResponseVotingPower = dRepInfoVotingPower
    , dRepInfoResponseDRepRegisterTxHash = HexText <$> dRepInfoDRepRegisterTx
    , dRepInfoResponseDRepRetireTxHash = HexText <$> dRepInfoDRepRetireTx
    , dRepInfoResponseSoleVoterRegisterTxHash = HexText <$> dRepInfoSoleVoterRegisterTx
    , dRepInfoResponseSoleVoterRetireTxHash = HexText <$> dRepInfoSoleVoterRetireTx
    , dRepInfoResponsePaymentAddress = dRepInfoPaymentAddress
    , dRepInfoResponseGivenName = dRepInfoGivenName
    , dRepInfoResponseObjectives = dRepInfoObjectives
    , dRepInfoResponseMotivations = dRepInfoMotivations
    , dRepInfoResponseQualifications = dRepInfoQualifications
    , dRepInfoResponseImageUrl = dRepInfoImageUrl
    , dRepInfoResponseImageHash = HexText <$> dRepInfoImageHash
    }

drepVotingPowerList :: App m => [Text] -> m [DRepVotingPowerListResponse]
drepVotingPowerList identifiers = do
  CacheEnv {dRepVotingPowerListCache} <- asks vvaCache
  
  let cacheKey = Text.intercalate "," (sort identifiers)
  
  results <- cacheRequest dRepVotingPowerListCache cacheKey $ 
    DRep.getDRepsVotingPowerList identifiers
  
  return $ map toDRepVotingPowerListResponse results
  where
    toDRepVotingPowerListResponse Types.DRepVotingPowerList{..} = 
      DRepVotingPowerListResponse
        { drepVotingPowerListResponseView = drepView
        , drepVotingPowerListResponseHashRaw = HexText drepHashRaw
        , drepVotingPowerListResponseVotingPower = drepVotingPower
        , drepVotingPowerListResponseGivenName = drepGivenName
        }

getCurrentDelegation :: App m => HexText -> m (Maybe DelegationResponse)
getCurrentDelegation (unHexText -> stakeKey) = do
  CacheEnv {adaHolderGetCurrentDelegationCache} <- asks vvaCache
  delegation <- cacheRequest adaHolderGetCurrentDelegationCache stakeKey $ AdaHolder.getCurrentDelegation stakeKey
  return $ delegationToResponse <$> delegation

getStakeKeyVotingPower :: App m => HexText -> m Integer
getStakeKeyVotingPower (unHexText -> stakeKey) = do
  CacheEnv {adaHolderVotingPowerCache} <- asks vvaCache
  cacheRequest adaHolderVotingPowerCache stakeKey $ AdaHolder.getStakeKeyVotingPower stakeKey


isProposalSearchedFor :: Maybe Text -> ProposalResponse -> Bool
isProposalSearchedFor Nothing _ = True
isProposalSearchedFor (Just searchQuery) (ProposalResponse{..}) = fromMaybe False $ do
          let normalisedSearchQuery = Text.toLower searchQuery
          let govActionId = unHexText proposalResponseTxHash <> "#" <> Text.pack (show proposalResponseIndex)
          let valuesToCheck = catMaybes
                [ Just govActionId
                , proposalResponseTitle
                , proposalResponseAbstract
                , proposalResponseMotivation
                , proposalResponseRationale
                ]

          pure $ any (\x -> normalisedSearchQuery `isInfixOf` Text.toLower x) valuesToCheck

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
        <$> getVotes drepId [] Nothing Nothing

  CacheEnv {proposalListCache} <- asks vvaCache

  proposals <- cacheRequest proposalListCache () (Proposal.listProposals mSearchQuery)

  mappedSortedAndFilteredProposals <- mapSortAndFilterProposals selectedTypes sortMode proposals
  let filteredProposals = filter
        ( \p@ProposalResponse {proposalResponseId} ->
            proposalResponseId `notElem` proposalsToRemove
            && isProposalSearchedFor mSearchQuery p
        ) mappedSortedAndFilteredProposals

  let total = length filteredProposals :: Int

  let elements = take pageSize $ drop (page * pageSize) filteredProposals

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
  proposal@Types.Proposal {proposalUrl, proposalDocHash} <- cacheRequest getProposalCache (unHexText govActionTxHash, govActionIndex) (Proposal.getProposal (unHexText govActionTxHash) govActionIndex)
  
  timeZone <- liftIO getCurrentTimeZone
  
  let proposalResponse = proposalToResponse timeZone proposal
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

getEnactedProposalDetails :: App m => Maybe GovernanceActionType -> m (Maybe EnactedProposalDetailsResponse)
getEnactedProposalDetails maybeType = do
  let proposalType = maybe "HardForkInitiation" governanceActionTypeToText maybeType
  
  mDetails <- Proposal.getPreviousEnactedProposal proposalType
  
  let response = enactedProposalDetailsToResponse <$> mDetails
  
  return response
  where
    governanceActionTypeToText :: GovernanceActionType -> Text
    governanceActionTypeToText actionType = 
      case actionType of
        HardForkInitiation -> "HardForkInitiation"
        ParameterChange -> "ParameterChange"
        _ -> "HardForkInitiation"
    
    enactedProposalDetailsToResponse :: Types.EnactedProposalDetails -> EnactedProposalDetailsResponse
    enactedProposalDetailsToResponse Types.EnactedProposalDetails{..} =
      EnactedProposalDetailsResponse
        { enactedProposalDetailsResponseId = enactedProposalDetailsId
        , enactedProposalDetailsResponseTxId = enactedProposalDetailsTxId
        , enactedProposalDetailsResponseIndex = enactedProposalDetailsIndex
        , enactedProposalDetailsResponseDescription = enactedProposalDetailsDescription
        , enactedProposalDetailsResponseHash = HexText enactedProposalDetailsHash
        }

getCurrentEpochParams :: App m => m GetCurrentEpochParamsResponse
getCurrentEpochParams = do
  CacheEnv {currentEpochCache} <- asks vvaCache
  GetCurrentEpochParamsResponse <$> cacheRequest currentEpochCache () Epoch.getCurrentEpochParams

getTransactionStatus :: App m => HexText -> m GetTransactionStatusResponse
getTransactionStatus (unHexText -> transactionId) = do
  status <- Transaction.getTransactionStatus transactionId
  return $ GetTransactionStatusResponse $ case status of
    Just value -> Just $ toJSON value
    Nothing    -> Nothing
    
throw500 :: App m => m ()
throw500 = throwError $ CriticalError "intentional system break for testing purposes"

getNetworkInfo :: App m => m GetNetworkInfoResponse
getNetworkInfo = do
  CacheEnv {networkInfoCache} <- asks vvaCache
  Types.NetworkInfo {..} <- Network.networkInfo
  return $ GetNetworkInfoResponse
    { getNetworkInfoResponseCurrentTime = networkInfoCurrentTime
    , getNetworkInfoResponseEpochNo = networkInfoEpochNo
    , getNetworkInfoResponseBlockNo = networkInfoBlockNo
    , getNetworkInfoResponseNetworkName = networkInfoNetworkName
    }

getNetworkTotalStake :: App m => m GetNetworkTotalStakeResponse
getNetworkTotalStake = do
  CacheEnv {networkTotalStakeCache} <- asks vvaCache
  Types.NetworkTotalStake {..} <- Network.networkTotalStake
  return $ GetNetworkTotalStakeResponse
    { getNetworkTotalStakeResponseTotalStakeControlledByDReps = networkTotalStakeControlledByDReps
    , getNetworkTotalStakeResponseTotalStakeControlledBySPOs = networkTotalStakeControlledBySPOs
    , getNetworkTotalStakeResponseAlwaysAbstainVotingPower = networkTotalAlwaysAbstainVotingPower
    , getNetworkTotalStakeResponseAlwaysNoConfidenceVotingPower = networkTotalAlwaysNoConfidenceVotingPower
    }

getNetworkMetrics :: App m => m GetNetworkMetricsResponse
getNetworkMetrics = do
  CacheEnv {networkMetricsCache} <- asks vvaCache
  Types.NetworkMetrics {..} <- Network.networkMetrics
  return $ GetNetworkMetricsResponse
    { getNetworkMetricsResponseUniqueDelegators = networkMetricsUniqueDelegators
    , getNetworkMetricsResponseTotalDelegations = networkMetricsTotalDelegations
    , getNetworkMetricsResponseTotalGovernanceActions = networkMetricsTotalGovernanceActions
    , getNetworkMetricsResponseTotalDRepVotes = networkMetricsTotalDRepVotes
    , getNetworkMetricsResponseTotalRegisteredDReps = networkMetricsTotalRegisteredDReps
    , getNetworkMetricsResponseTotalDRepDistr = networkMetricsTotalDRepDistr
    , getNetworkMetricsResponseTotalActiveDReps = networkMetricsTotalActiveDReps
    , getNetworkMetricsResponseTotalInactiveDReps = networkMetricsTotalInactiveDReps
    , getNetworkMetricsResponseTotalActiveCIP119CompliantDReps = networkMetricsTotalActiveCIP119CompliantDReps
    , getNetworkMetricsResponseTotalRegisteredDirectVoters = networkMetricsTotalRegisteredDirectVoters
    , getNetworkMetricsResponseNoOfCommitteeMembers = networkMetricsNoOfCommitteeMembers
    , getNetworkMetricsResponseQuorumNumerator = networkMetricsQuorumNumerator
    , getNetworkMetricsResponseQuorumDenominator = networkMetricsQuorumDenominator
    }

getAccountInfo :: App m => HexText -> m GetAccountInfoResponse
getAccountInfo (unHexText -> stakeKey) = do
  CacheEnv {accountInfoCache} <- asks vvaCache
  Types.AccountInfo {..} <- Account.accountInfo stakeKey
  return $ GetAccountInfoResponse
    { getAccountInfoResponseId = accountInfoId
    , getAccountInfoResponseView = accountInfoView
    , getAccountInfoResponseIsRegistered = accountInfoIsRegistered
    , getAccountInfoResponseIsScriptBased = accountInfoIsScriptBased
    }
