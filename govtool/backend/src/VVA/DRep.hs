{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module VVA.DRep where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader

import           Crypto.Hash

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as C
import           Data.FileEmbed             (embedFile)
import           Data.Foldable              (Foldable (sum))
import           Data.Has                   (Has)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, isJust, isNothing)
import           Data.Scientific
import           Data.String                (fromString)
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Encoding         as Text
import           Data.Time

import qualified Database.PostgreSQL.Simple as SQL

import           VVA.Config
import           VVA.Pool                   (ConnectionPool, withPool)
import qualified VVA.Proposal               as Proposal
import           VVA.Types                  (AppError, DRepInfo (..), DRepRegistration (..), DRepStatus (..),
                                             DRepType (..), Proposal (..), Vote (..))

sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getVotingPowerSql :: SQL.Query
getVotingPowerSql = sqlFrom $(embedFile "sql/get-voting-power.sql")

getVotingPower ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m) =>
  Text ->
  m Integer
getVotingPower drepId = withPool $ \conn -> do
  result <-
    liftIO
      (SQL.query @_ @(SQL.Only Scientific) conn getVotingPowerSql $ SQL.Only drepId)
  case result of
    [SQL.Only votingPower] -> return $ floor votingPower
    []                     -> return 0

listDRepsSql :: SQL.Query
listDRepsSql = sqlFrom $(embedFile "sql/list-dreps.sql")

listDReps ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  m [DRepRegistration]
listDReps = withPool $ \conn -> do
  results <- liftIO $ SQL.query_ conn listDRepsSql
  timeZone <- liftIO getCurrentTimeZone
  return
    [ DRepRegistration drepHash drepView url dataHash (floor @Scientific deposit) votingPower status drepType txHash (localTimeToUTC timeZone date) metadataError paymentAddress givenName objectives motivations qualifications imageUrl imageHash
    | ( drepHash
        , drepView
        , url
        , dataHash
        , deposit
        , votingPower
        , isActive
        , txHash
        , date
        , latestDeposit
        , latestNonDeregisterVotingAnchorWasNotNull
        , metadataError
        , paymentAddress
        , givenName
        , objectives
        , motivations
        , qualifications
        , imageUrl
        , imageHash
      ) <- results
    , let status = case (isActive, deposit) of
                      (_, d)        | d < 0 -> Retired
                      (isActive, d) | d >= 0 && isActive -> Active
                                    | d >= 0 && not isActive -> Inactive
    , let latestDeposit' = floor @Scientific latestDeposit :: Integer
    , let drepType | latestDeposit' >= 0 && isNothing url = SoleVoter
                   | latestDeposit' >= 0 && isJust url = DRep
                   | latestDeposit' < 0 && not latestNonDeregisterVotingAnchorWasNotNull = SoleVoter
                   | latestDeposit' < 0 && latestNonDeregisterVotingAnchorWasNotNull = DRep
                   | Data.Maybe.isJust url = DRep
    ]

getVotesSql :: SQL.Query
getVotesSql = sqlFrom $(embedFile "sql/get-votes.sql")

getVotes ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m, MonadError AppError m) =>
  Text ->
  [Text] ->
  m ([Vote], [Proposal])
getVotes drepId selectedProposals = withPool $ \conn -> do
  results <- liftIO $ SQL.query conn getVotesSql (SQL.Only drepId)
  let proposalsToSelect = if null selectedProposals
                          then [ govActionId | (_, govActionId, _, _, _, _, _, _, _) <- results]
                          else selectedProposals
  proposals <- Proposal.getProposals (Just proposalsToSelect)
  let proposalMap = M.fromList $ map (\x -> (proposalId x, x)) proposals
  timeZone <- liftIO getCurrentTimeZone
  return
    ([ Vote proposalId' drepId' vote' url' docHash' epochNo' (localTimeToUTC timeZone date') voteTxHash'
      | (proposalId', govActionId', drepId', vote', url', docHash', epochNo', date', voteTxHash') <- results
      , govActionId' `elem` proposalsToSelect
    ], proposals)

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
    [ ( isRegisteredAsDRep
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
        { dRepInfoIsRegisteredAsDRep = fromMaybe False isRegisteredAsDRep
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
    [] -> return $ DRepInfo False False False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
