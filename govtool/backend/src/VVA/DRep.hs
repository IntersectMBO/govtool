{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module VVA.DRep where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Crypto.Hash
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Scientific
import Data.String (fromString)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as SQL
import VVA.Config
import qualified VVA.Proposal as Proposal
import Data.Foldable (Foldable(sum))
import Data.Has (Has)
import VVA.Pool (ConnectionPool, withPool)
import VVA.Types
  ( AppError
  , DRepRegistration(..)
  , Proposal(..)
  , Vote(..)
  , DRepInfo(..)
  )




sqlFrom :: ByteString -> SQL.Query
sqlFrom bs = fromString $ unpack $ Text.decodeUtf8 bs

getVotingPowerSql :: SQL.Query
getVotingPowerSql = sqlFrom $(embedFile "sql/get-voting-power.sql")

getVotingPower ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m, MonadFail m) =>
  Text ->
  m Integer
getVotingPower drepId = withPool $ \conn -> do
  [SQL.Only votingPower] <-
    liftIO
      (SQL.query @_ @(SQL.Only Scientific) conn getVotingPowerSql $ SQL.Only drepId)
  return $ floor votingPower

listDRepsSql :: SQL.Query
listDRepsSql = sqlFrom $(embedFile "sql/list-dreps.sql")

listDReps ::
  (Has ConnectionPool r, Has VVAConfig r, MonadReader r m, MonadIO m) =>
  m [DRepRegistration]
listDReps = withPool $ \conn -> do
  results <- liftIO $ SQL.query_ conn listDRepsSql
  return [DRepRegistration drepHash url dataHash (floor @Scientific deposit) | (drepHash, url, dataHash, deposit) <- results]

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
                          then [ govActionId | (_, govActionId, _, _, _, _) <- results]
                          else selectedProposals
  proposals <- Proposal.getProposals (Just proposalsToSelect)
  let proposalMap = M.fromList $ map (\x -> (proposalId x, x)) proposals
  return
    ([ Vote proposalId' drepId' vote' url' docHash'
      | (proposalId', govActionId', drepId', vote', url', docHash') <- results
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
    [(isRegistered, wasRegistered, deposit)] ->
      return $ DRepInfo
        { dRepInfoIsRegistered = fromMaybe False isRegistered
        , dRepInfoWasRegistered = fromMaybe False wasRegistered
        , dRepInfoDeposit = deposit
        }
    [] -> return $ DRepInfo False False Nothing