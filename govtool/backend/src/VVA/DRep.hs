{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module VVA.DRep where

import           Control.Monad.Except               (MonadError)
import           Control.Monad.Reader

import           Crypto.Hash

import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
import qualified  Data.ByteString.Base16             as Base16
import qualified  Data.ByteString.Char8              as C
import           Data.FileEmbed                     (embedFile)
import           Data.Foldable                      (Foldable (sum))
import           Data.Has                           (Has)
import qualified  Data.Map                           as M
import           Data.Maybe                         (fromMaybe, isJust, isNothing)
import           Data.Scientific
import           Data.String                        (fromString)
import           Data.Text                          (Text, pack, unpack, intercalate)
import qualified  Data.Text.Encoding                 as Text
import           Data.Time

import qualified  Database.PostgreSQL.Simple         as SQL
import           Database.PostgreSQL.Simple.Types   (In(..))
import           Database.PostgreSQL.Simple.FromRow

import           VVA.Config
import           VVA.Pool                           (ConnectionPool, withPool)
import qualified  VVA.Proposal                       as Proposal
import           VVA.Types                          (AppError, DRepInfo (..), DRepRegistration (..), DRepStatus (..),
                                                     DRepType (..), Proposal (..), Vote (..), DRepVotingPowerList (..))

data DRepQueryResult = DRepQueryResult
  { queryDrepHash :: Text
  , queryDrepView :: Text
  , queryIsScriptBased :: Bool
  , queryUrl :: Maybe Text
  , queryDataHash :: Maybe Text
  , queryDeposit :: Scientific
  , queryVotingPower :: Maybe Integer
  , queryIsActive :: Bool
  , queryTxHash :: Maybe Text
  , queryDate :: LocalTime
  , queryLatestDeposit :: Scientific
  , queryLatestNonDeregisterVotingAnchorWasNotNull :: Bool
  , queryMetadataError :: Maybe Text
  , queryPaymentAddress :: Maybe Text
  , queryGivenName :: Maybe Text
  , queryObjectives :: Maybe Text
  , queryMotivations :: Maybe Text
  , queryQualifications :: Maybe Text
  , queryImageUrl :: Maybe Text
  , queryImageHash :: Maybe Text
  , queryIdentityReferences :: Maybe Value
  , queryLinkReferences :: Maybe Value
  } deriving (Show)

instance FromRow DRepQueryResult where
  fromRow = DRepQueryResult 
    <$> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

listDRepsSql :: SQL.Query
listDRepsSql = sqlFrom $(embedFile "sql/list-dreps.sql")
listDReps ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  Maybe Text -> m [DRepRegistration]
listDReps mSearchQuery = withPool $ \conn -> do
  let searchParam = fromMaybe "" mSearchQuery
  results <- liftIO (SQL.query conn listDRepsSql
    ( searchParam -- COALESCE(?, '')
    , searchParam -- LENGTH(?)
    , searchParam -- AND ?
    , searchParam -- decode(?, 'hex')
    , "%" <> searchParam <> "%" -- dh.view
    , "%" <> searchParam <> "%" -- given_name
    , "%" <> searchParam <> "%" -- payment_address
    , "%" <> searchParam <> "%" -- objectives
    , "%" <> searchParam <> "%" -- motivations
    , "%" <> searchParam <> "%" -- qualifications
    ) :: IO [DRepQueryResult])

  timeZone <- liftIO getCurrentTimeZone
  return
    [ DRepRegistration 
      (queryDrepHash result)
      (queryDrepView result)
      (queryIsScriptBased result)
      (queryUrl result)
      (queryDataHash result)
      (floor @Scientific $ queryDeposit result)
      (queryVotingPower result)
      status
      drepType
      (queryTxHash result)
      (localTimeToUTC timeZone $ queryDate result)
      (queryMetadataError result)
      (queryPaymentAddress result)
      (queryGivenName result)
      (queryObjectives result)
      (queryMotivations result)
      (queryQualifications result)
      (queryImageUrl result)
      (queryImageHash result)
      (queryIdentityReferences result)
      (queryLinkReferences result)
    | result <- results
    , let status = case (queryIsActive result, queryDeposit result) of
                      (_, d)        | d < 0 -> Retired
                      (isActive, d) | d >= 0 && isActive -> Active
                                    | d >= 0 && not isActive -> Inactive
    , let latestDeposit' = floor @Scientific (queryLatestDeposit result) :: Integer
    , let drepType | latestDeposit' >= 0 && isNothing (queryUrl result) = SoleVoter
                   | latestDeposit' >= 0 && isJust (queryUrl result) = DRep
                   | latestDeposit' < 0 && not (queryLatestNonDeregisterVotingAnchorWasNotNull result) = SoleVoter
                   | latestDeposit' < 0 && queryLatestNonDeregisterVotingAnchorWasNotNull result = DRep
                   | Data.Maybe.isJust (queryUrl result) = DRep
    ]
    
getVotingPowerSql :: SQL.Query
getVotingPowerSql = sqlFrom $(embedFile "sql/get-voting-power.sql")

getVotingPower ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m) =>
  Text ->
  m Integer
getVotingPower drepId = withPool $ \conn -> do
  result <- liftIO (SQL.query @_ @(SQL.Only Scientific) conn getVotingPowerSql $ SQL.Only drepId)
  case result of
    [SQL.Only votingPower] -> return $ floor votingPower
    []                     -> return 0

getVotesSql :: SQL.Query
getVotesSql = sqlFrom $(embedFile "sql/get-votes.sql")

getVotes ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Text ->
  [Text] ->
  m ([Vote], [Proposal])
getVotes drepId selectedProposals = withPool $ \conn -> do
  results <- liftIO $ SQL.query conn getVotesSql (SQL.Only drepId)

  if null results
    then return ([], [])
    else do
      let proposalsToSelect = if null selectedProposals
                              then [ govActionId | (_, govActionId, _, _, _, _, _, _, _) <- results]
                              else selectedProposals
      allProposals <- mapM (Proposal.getProposals . Just . (:[])) proposalsToSelect
      let proposals = concat allProposals

      let proposalMap = M.fromList $ map (\x -> (proposalId x, x)) proposals

      timeZone <- liftIO getCurrentTimeZone

      let votes = 
            [ Vote proposalId' govActionId' drepId' vote' url' docHash' epochNo' (localTimeToUTC timeZone date') voteTxHash'
            | (proposalId', govActionId', drepId', vote', url', docHash', epochNo', date', voteTxHash') <- results
            , govActionId' `elem` proposalsToSelect
            ]

      return (votes, proposals)

getDRepInfoSql :: SQL.Query
getDRepInfoSql = sqlFrom $(embedFile "sql/get-drep-info.sql")

getDRepInfo
  :: ( Has ConnectionPool r
     , Has VVAConfig r
     , MonadReader r m
     , MonadIO m
     , MonadFail m
     , MonadError AppError m
     )
  => Text
  -> m DRepInfo
getDRepInfo drepId = withPool $ \conn -> do
  result <- liftIO $ SQL.query conn getDRepInfoSql (SQL.Only drepId)
  case result of
    [ ( isScriptBased
      , isRegisteredAsDRep
      , wasRegisteredAsDRep
      , isRegisteredAsSoleVoter
      , wasRegisteredAsSoleVoter
      , deposit
      , url
      , dataHash
      , votingPower
      , drepRegisterTx
      , drepRetireTx
      , soleVoterRegisterTx
      , soleVoterRetireTx
      , paymentAddress
      , givenName
      , objectives
      , motivations
      , qualifications
      , imageUrl
      , imageHash
      )] ->
      return $ DRepInfo
        { dRepInfoIsScriptBased = isScriptBased
        , dRepInfoIsRegisteredAsDRep = fromMaybe False isRegisteredAsDRep
        , dRepInfoWasRegisteredAsDRep = fromMaybe False wasRegisteredAsDRep
        , dRepInfoIsRegisteredAsSoleVoter = fromMaybe False isRegisteredAsSoleVoter
        , dRepInfoWasRegisteredAsSoleVoter = fromMaybe False wasRegisteredAsSoleVoter
        , dRepInfoDeposit = deposit
        , dRepInfoUrl = url
        , dRepInfoDataHash = dataHash
        , dRepInfoVotingPower = votingPower
        , dRepInfoDRepRegisterTx = drepRegisterTx
        , dRepInfoDRepRetireTx = drepRetireTx
        , dRepInfoSoleVoterRegisterTx = soleVoterRegisterTx
        , dRepInfoSoleVoterRetireTx = soleVoterRetireTx
        , dRepInfoPaymentAddress = paymentAddress
        , dRepInfoGivenName = givenName
        , dRepInfoObjectives = objectives
        , dRepInfoMotivations = motivations
        , dRepInfoQualifications = qualifications
        , dRepInfoImageUrl = imageUrl
        , dRepInfoImageHash = imageHash
        }
    [] -> return $ DRepInfo False False False False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    _ -> error "Unexpected result from database query in getDRepInfo"

getAllDRepsVotingPowerSql :: SQL.Query
getAllDRepsVotingPowerSql = sqlFrom $(embedFile "sql/get-dreps-voting-power-list.sql")

getFilteredDRepVotingPowerSql :: SQL.Query
getFilteredDRepVotingPowerSql = sqlFrom $(embedFile "sql/get-filtered-dreps-voting-power.sql")

getDRepsVotingPowerList ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  [Text] ->
  m [DRepVotingPowerList]
getDRepsVotingPowerList identifiers = withPool $ \conn -> do
  results <- if null identifiers
    then do
      liftIO $ SQL.query_ conn getAllDRepsVotingPowerSql
    else do
      resultsPerIdentifier <- forM identifiers $ \identifier -> do
        liftIO $ SQL.query conn getFilteredDRepVotingPowerSql (identifier, identifier)
      
      return $ concat resultsPerIdentifier
  
  return
    [ DRepVotingPowerList view hashRaw votingPower givenName
    | (view, hashRaw, votingPower', givenName) <- results
    , let votingPower = floor @Scientific votingPower'
    ]